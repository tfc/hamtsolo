{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Conduit
import           Control.Exception
import           Control.Monad              (unless, void, when)
import           Data.Attoparsec.Binary     (anyWord16le)
import qualified Data.Attoparsec.ByteString as A
import           Data.Binary                (Word16, Word8, encode)
import           Data.Bits                  (testBit)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as B2
import           Data.ByteString.Lazy       (toStrict)
import           Data.Conduit.Attoparsec    (ParseError, PositionRange,
                                             conduitParser, conduitParserEither,
                                             sinkParser)
import           Data.Conduit.Network
import           Data.Monoid                ((<>))
import           Data.Typeable
import qualified Options.Applicative        as O

data SolInitSequenceFail = SolInitSequenceFail String deriving (Show, Typeable)
instance Exception SolInitSequenceFail

data SolPacket = HeartBeat Int
    | SolData ByteString
    | SolControl {
        ctrlRTS          :: Bool,
        ctrlDTR          :: Bool,
        ctrlBreak        :: Bool,
        statusTxOverflow :: Bool,
        statusLoopback   :: Bool,
        statusPower      :: Bool,
        statusRxFlushTO  :: Bool,
        statusTestMode   :: Bool
      }
    deriving (Show)

solParser :: A.Parser SolPacket
solParser = A.choice [
    do { A.word8 0x2b; A.take 3; n <- anyWord16le; A.take 2; return $ HeartBeat $ fromIntegral n },
    do { A.word8 0x2a; A.take 7; n <- anyWord16le; s <- A.take $ fromIntegral n; return $ SolData s },
    do { A.word8 0x29; A.take 7; control <- A.anyWord8; status <- A.anyWord8;
         return $ SolControl (testBit control 0) (testBit control 1) (testBit control 2)
                             (testBit status  0) (testBit status  1) (testBit status  2)
                             (testBit status  3) (testBit status  4)
       }
    ]

authMsg :: ByteString -> ByteString -> ByteString
authMsg u p = let
    lu = fromIntegral (B.length u) :: Word8
    lp = fromIntegral (B.length p) :: Word8
    lg = 2 + lu + lp
    in
    B.concat [ B.pack $ [0x13, 0, 0, 0, 1, lg, 0, 0, 0, lu], u, B.pack [lp], p ]

startSolMsg :: ByteString
startSolMsg = let
    maxTxBuffer            = 1000  :: Word16
    txBufferTimeout        = 100   :: Word16
    txOverflowTimeout      = 0     :: Word16
    hostSessionRxTimeout   = 10000 :: Word16
    hostFifoRxFlushTimeout = 0     :: Word16
    heartbeatInterval      = 5000  :: Word16
    in
    B.concat [ B.pack [0x20, 0, 0, 0, 0, 0, 0, 0],
        B.concat $ map (B.reverse . toStrict . encode)
            ([maxTxBuffer, txBufferTimeout, txOverflowTimeout, hostSessionRxTimeout,
              hostFifoRxFlushTimeout, heartbeatInterval] :: [Word16]),
        B.pack [0, 0, 0, 0]]

sayHello :: Conduit ByteString IO ByteString
sayHello = yield $ B.pack [0x10, 0x00, 0x00, 0x00, 0x53, 0x4f, 0x4c, 0x20]

okPacket :: Word8 -> Int -> A.Parser Bool
okPacket x n = do { A.word8 x; bad <- A.anyWord8; A.take (n - 2); return $ bad == 0 }

acceptPacketOrThrow :: A.Parser Bool -> String -> Conduit ByteString IO ByteString
acceptPacketOrThrow p errStr = do
    packetGood <- sinkParser p
    unless packetGood $ throwM $ SolInitSequenceFail errStr

reactPrologue :: String -> String -> Conduit ByteString IO ByteString
reactPrologue user pass = do
    acceptPacketOrThrow (okPacket 0x11 13) "Server does not accept redirection request."
    yield $ authMsg (B2.pack user) (B2.pack pass)

    acceptPacketOrThrow (okPacket 0x14 9) "Server does not accept authentication."
    yield startSolMsg

    acceptPacketOrThrow (okPacket 0x21 23) "Authenticated, but Server refuses SOL."

reactSolMode :: Conduit (Either ParseError (PositionRange, SolPacket)) IO ByteString
reactSolMode = do
    x <- await
    case x of
        Nothing -> void (liftIO $ putStrLn "Server closed the connection.")
        Just m -> case m of
            Left e -> liftIO $ putStrLn $ "parse err: " ++ show e
            Right (_, msg) -> case msg of
                HeartBeat  n -> yield $ B.pack [0x2b, 0, 0, 0, 2, 0, 0, 0]
                SolData    s -> liftIO $ B.putStr s
                SolControl rts dtr brk txOF loopB power rxFlTO testMode  -> liftIO $ do
                    when rts   $ putStrLn "SOL: RTS asserted on serial"
                    when dtr   $ putStrLn "SOL: DTR asserted on serial"
                    when brk   $ putStrLn "SOL: BRK asserted on serial"
                    when power $ putStrLn "SOL: power state change"
                    when loopB $ putStrLn "SOL: loopback mode activated"
                    return ()
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
        liftIO $ putStrLn "Authenticated. SOL active."
        (fromClient2, ()) <- fromClient $$++ reactPrologue user pass =$ appSink server
        fromClient2 $$+- (conduitParserEither solParser =$= reactSolMode) =$ appSink server
