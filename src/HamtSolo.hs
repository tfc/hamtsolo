{-# LANGUAGE OverloadedStrings #-}
import           Conduit
import           Control.Monad            (void, when)
import           Data.Attoparsec.Binary   (anyWord16le)
import qualified Data.Attoparsec.ByteString as A
import           Data.Binary              (encode, Word8, Word16)
import           Data.ByteString          (ByteString, pack)
import qualified Data.ByteString          as B
import           Data.ByteString.Lazy     (toStrict)
import           Data.Conduit.Network
import           Data.Conduit.Attoparsec  (conduitParser, PositionRange)

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

reactPrologue :: Conduit (PositionRange, PrologueHostAnswer) IO ByteString
reactPrologue = do
    Just (_, Redirection True) <- await
    yield $ auth_msg "admin" "Password123!"
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

main :: IO ()
main =
    runTCPClient (clientSettings 16994 "192.168.157") $ \server -> do
        liftIO $ putStrLn "Connected. Authenticating."
        (fromClient, ()) <- appSource server $$+ sayHello =$ appSink server
        (fromClient2, ()) <- fromClient $$++ (conduitParser prologueParser =$= reactPrologue) =$ appSink server
        fromClient2 $$+- (conduitParser solParser =$= reactSolMode) =$ appSink server
