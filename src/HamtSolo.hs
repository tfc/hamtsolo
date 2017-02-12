{-# LANGUAGE OverloadedStrings #-}
import           Conduit
import           Control.Monad              (void, when)
import           Data.Attoparsec.Binary     (anyWord16le)
import qualified Data.Attoparsec.ByteString as A
import           Data.Binary                (encode, Word8, Word16)
import           Data.ByteString            (ByteString, pack)
import qualified Data.ByteString.Char8      as B2
import qualified Data.ByteString            as B
import           Data.ByteString.Lazy       (toStrict)
import           Data.Conduit.Network
import           Data.Conduit.Attoparsec    (conduitParser, PositionRange)
import qualified Options.Applicative        as O
import Data.Monoid ((<>))

data PrologueHostAnswer = Redirection Bool | Authentication Bool | SolStart Bool
    deriving (Show)

data SolPacket = HeartBeat Int | SolData ByteString | SolControl
    deriving (Show)

okPacket x n f = do { A.word8 x; bad <- A.anyWord8; A.take (n - 2); return $ f (bad == 0) }

prologueParser :: A.Parser PrologueHostAnswer
prologueParser = 
    A.choice [okPacket 0x11 13 (\x -> Redirection x),
              okPacket 0x14  9 (\x -> Authentication x),
              okPacket 0x21 23 (\x -> SolStart x)]

solParser :: A.Parser SolPacket
solParser = A.choice [ 
    do { A.word8 0x2b; A.take 3; n <- anyWord16le; A.take 2; return $ HeartBeat $ fromIntegral n },
    do { A.word8 0x2a; A.take 7; n <- anyWord16le; s <- A.take $ fromIntegral n; return $ SolData s },
    do { A.word8 0x29; A.take 9; return SolControl }
    ]

auth_msg :: ByteString -> ByteString -> ByteString
auth_msg u p = let
    lu = B.length u
    lp = B.length p
    lg = 2 + lu + lp
    in
    B.concat [ pack $ map fromIntegral [0x13, 0, 0, 0, 1, lg, 0, 0, 0, lu], 
               u, pack [fromIntegral lp], p ]

startSolMsg = let
    maxTxBuffer            = 1000  :: Word16
    txBufferTimeout        = 100   :: Word16
    txOverflowTimeout      = 0     :: Word16
    hostSessionRxTimeout   = 10000 :: Word16
    hostFifoRxFlushTimeout = 0     :: Word16
    heartbeatInterval      = 5000  :: Word16
    in
    B.concat [ pack $ map fromIntegral  [0x20, 0, 0, 0, 0, 0, 0, 0],
        B.concat $ map (B.reverse . toStrict . encode)
            ([maxTxBuffer, txBufferTimeout, txOverflowTimeout, hostSessionRxTimeout, 
              hostFifoRxFlushTimeout, heartbeatInterval] :: [Word16]),
        pack $ map fromIntegral [0, 0, 0, 0]]

sayHello :: Conduit ByteString IO ByteString
sayHello = yield $ pack [0x10, 0x00, 0x00, 0x00, 0x53, 0x4f, 0x4c, 0x20]

reactPrologue :: String -> String -> Conduit (PositionRange, PrologueHostAnswer) IO ByteString
reactPrologue user pass = do
    Just (_, Redirection True) <- await
    yield $ auth_msg (B2.pack user) (B2.pack pass)
    Just (_, Authentication True) <- await
    yield startSolMsg
    Just (_, SolStart True) <- await
    liftIO $ putStrLn "Authenticated. SOL active."

reactSolMode :: Conduit (PositionRange, SolPacket) IO ByteString
reactSolMode = do
    Just (_, msg) <- await
    case msg of
        HeartBeat  n -> yield $ pack [0x2b, 0, 0, 0, 2, 0, 0, 0]
        SolData    s -> liftIO $ B.putStr s
        SolControl   -> liftIO $ putStrLn "CONTROL msg from server"
    reactSolMode

data CLArguments = CLArguments { user :: String, pass :: String, port :: Int, host :: String }

main :: IO ()
main = let
    parser = CLArguments 
        <$> O.option O.str   ( O.short 'u' <> O.long "user" <> O.value "admin" <> O.metavar "<user>" <>
                               O.help "Authentication user name" <> O.showDefault)
        <*> O.option O.str   ( O.short 'p' <> O.long "pass" <> O.value "Password123!" <> O.metavar "<password>" <>
                               O.help "Authentication password" <> O.showDefault)
        <*> O.option O.auto  ( O.long "port" <> O.value 16994 <> O.metavar "<port>" <>
                               O.help "TCP connection port" <> O.showDefault)
        <*> O.argument O.str ( O.metavar "<host>" <> O.help "AMT host to connect to")
    opts = O.info (O.helper <*> parser)
      ( O.fullDesc <> O.header "hamtsolo - An Intel AMT Serial-Over-LAN (SOL) client" )
    in do
    (CLArguments user pass port host) <- O.execParser opts
    runTCPClient (clientSettings port $ B2.pack host) $ \server -> do
        liftIO $ putStrLn "Connected. Authenticating."
        (fromClient, ()) <- appSource server $$+ sayHello =$ appSink server
        (fromClient2, ()) <- fromClient $$++ (conduitParser prologueParser =$= (reactPrologue user pass)) =$ appSink server
        fromClient2 $$+- (conduitParser solParser =$= reactSolMode) =$ appSink server
