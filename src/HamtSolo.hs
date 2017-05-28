{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Exception            (Exception, bracket, try)
import           Control.Monad                (unless, when)
import           Control.Monad.Catch          (throwM)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Attoparsec.Binary       (anyWord16le)
import qualified Data.Attoparsec.ByteString   as A
import           Data.Binary                  (Word16, Word8, encode)
import           Data.Bits                    (testBit)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as B2
import           Data.ByteString.Lazy         (toStrict)
import           Data.Conduit
import           Data.Conduit.Attoparsec      (conduitParser, sinkParser)
import qualified Data.Conduit.Combinators     as CC
import qualified Data.Conduit.Internal        as CI
import           Data.Conduit.Network
import qualified Data.Conduit.TMChan          as TMC
import           Data.Maybe                   (fromJust, isJust)
import           Data.Monoid                  ((<>))
import           Data.Typeable
import           GHC.IO.Exception             (IOException)
import qualified Options.Applicative          as O
import           System.IO                    (BufferMode (..), hPutStrLn,
                                               hSetBuffering, stderr, stdin,
                                               stdout)
import           System.Posix.IO              (stdInput)
import qualified System.Posix.Terminal        as T

data SolException = SolException String deriving (Show, Typeable)
instance Exception SolException

data SolPacket =
      HeartBeat Int
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
    | UserMsgToHost ByteString
        -- Use bytestring instead of Word8 because maybe we want to send
        -- line buffered strings in the future.
    | UserQuit
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

userParser :: A.Parser SolPacket
userParser = A.choice [
    A.word8 0x1d *> return UserQuit,
    UserMsgToHost . B.singleton <$> A.anyWord8
    ]

authMsg :: ByteString -> ByteString -> ByteString
authMsg u p = let
    lu = fromIntegral (B.length u) :: Word8
    lp = fromIntegral (B.length p) :: Word8
    lg = 2 + lu + lp
    in
    B.concat [ B.pack [0x13, 0, 0, 0, 1, lg, 0, 0, 0, lu], u, B.pack [lp], p ]

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
            [maxTxBuffer, txBufferTimeout, txOverflowTimeout, hostSessionRxTimeout, hostFifoRxFlushTimeout, heartbeatInterval],
        B.pack [0, 0, 0, 0]]

sayHello :: Conduit ByteString IO ByteString
sayHello = yield $ B.pack [0x10, 0x00, 0x00, 0x00, 0x53, 0x4f, 0x4c, 0x20]

okPacket :: Word8 -> Int -> A.Parser Bool
okPacket x n = do { A.word8 x; bad <- A.anyWord8; A.take (n - 2); return $ bad == 0 }

userMsgPacket :: ByteString -> ByteString
-- TODO: send word16 with actual length instead of [1, 0]
userMsgPacket bs = let
        patchedMsg = B.map (\c -> if c == 0xa then 0xd else c) bs -- transform LF to CR
    in B.concat [ B.pack [0x28, 0, 0, 0, 0, 0, 0, 0], B.pack [1, 0], patchedMsg]

acceptPacketOrThrow :: String -> A.Parser Bool -> Conduit ByteString IO ByteString
acceptPacketOrThrow errStr p = do
    packetGood <- sinkParser p
    unless packetGood $ throwM $ SolException errStr

reactPrologue :: String -> String -> Conduit ByteString IO ByteString
reactPrologue user pass = do
    acceptPacketOrThrow "Server does not accept redirection request." $ okPacket 0x11 13
    yield $ authMsg (B2.pack user) (B2.pack pass)

    acceptPacketOrThrow "Server does not accept authentication." $ okPacket 0x14 9
    yield startSolMsg

    acceptPacketOrThrow "Authenticated, but Server refuses SOL." $ okPacket 0x21 23

printInfo :: String -> IO ()
printInfo = hPutStrLn stderr

reactSolMode :: Conduit SolPacket IO ByteString
reactSolMode = awaitForever $ \x ->
    case x of
        HeartBeat  n -> yield $ B.pack [0x2b, 0, 0, 0, 2, 0, 0, 0]
        SolData    s -> liftIO $ B.putStr s
        SolControl rts dtr brk txOF loopB power rxFlTO testMode  -> liftIO $ do
            when rts   $ printInfo "SOL: RTS asserted on serial"
            when dtr   $ printInfo "SOL: DTR asserted on serial"
            when brk   $ printInfo "SOL: BRK asserted on serial"
            when power $ printInfo "SOL: power state change"
            when loopB $ printInfo "SOL: loopback mode activated"
            return ()
        UserQuit -> throwM $ SolException "Seen ^]. Quitting app."
        UserMsgToHost m -> yield $ userMsgPacket m

withTerminalSettings :: IO r -> IO r
withTerminalSettings runStuff = let
    setStdinAttrs a = T.setTerminalAttributes stdInput a T.WhenFlushed
    in
    bracket
    (do
        eOldSettings :: Either IOException T.TerminalAttributes <- try $ T.getTerminalAttributes stdInput
        case eOldSettings of
            Left _ -> return Nothing
            Right oldSettings -> do
                let newSettings = flip T.withMinInput 1
                                $ flip T.withTime     0
                                $ foldr id oldSettings $ flip T.withoutMode
                                    <$> [T.KeyboardInterrupts, T.EnableEcho, T.ProcessInput]

                setStdinAttrs newSettings
                return $ Just oldSettings
    )
    (mapM_ setStdinAttrs)
    (const runStuff)

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
      ( O.fullDesc
      <> O.progDesc "hamtsolo lets you connect to Intel computers with enabled \
                     \AMT and establish a serial-over-lan (SOL) connection."
      <> O.header "hamtsolo - An Intel AMT Serial-Over-LAN (SOL) client" )
    in do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    (CLArguments user pass port host) <- O.execParser opts
    runTCPClient (clientSettings port $ B2.pack host) $ \server -> do
        liftIO $ printInfo "Connected. Authenticating."
        (fromClient, ()) <- appSource server $$+ sayHello =$ appSink server
        liftIO $ printInfo "Authenticated. SOL active."
        withTerminalSettings $ do
            (fromClient2, ()) <- fromClient $$++ reactPrologue user pass =$ appSink server
            (clientSource, clientFinalizer) <- CI.unwrapResumable fromClient2

            let sckIn = transPipe liftIO (clientSource =$= conduitParser solParser)
            let kbdIn = transPipe liftIO (CC.stdin     =$= conduitParser userParser)

            runResourceT $ do
                sources <- TMC.mergeSources [sckIn, kbdIn] 2
                sources =$= awaitForever (yield . snd) $$ transPipe liftIO (reactSolMode =$= appSink server)
