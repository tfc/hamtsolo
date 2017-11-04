{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
import           Control.Applicative          ((<|>))
import           Control.Concurrent           (threadDelay)
import           Control.Concurrent.Async
import           Control.Exception            (Exception, bracket, finally, try)
import           Control.Monad                (unless, void, when)
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
import           Data.IORef
import           Data.Maybe                   (fromJust, isJust)
import           Data.Monoid                  ((<>))
import           Data.Typeable
import           Development.GitRev
import           GHC.Conc.Sync                (atomically)
import           GHC.IO.Exception             (IOException)
import           Network                      (PortID (PortNumber), PortNumber,
                                               accept, listenOn, sClose)
import qualified Options.Applicative          as O
import           System.Exit                  (die, exitSuccess)
import           System.IO                    (BufferMode (..), hPutStrLn,
                                               hSetBuffering, stderr, stdin,
                                               stdout)
import           System.Posix.IO              (stdInput)
import qualified System.Posix.Terminal        as T

data SolException = UserQuitException
                  | SolException String
    deriving (Show)
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
    HeartBeat . fromIntegral <$> (A.word8 0x2b *> A.take 3 *> anyWord16le <* A.take 2),
    SolData <$> (A.word8 0x2a *> A.take 7 *> ((A.take . fromIntegral) =<< anyWord16le)),
    (\control status -> SolControl (testBit control 0) (testBit control 1) (testBit control 2)
                                  (testBit status  0) (testBit status  1) (testBit status  2)
                                  (testBit status  3) (testBit status  4))
        <$> (A.word8 0x29 *> A.take 7 *> A.anyWord8) <*> A.anyWord8
    ]

userParser :: A.Parser SolPacket
userParser = A.choice [
    const UserQuit <$> A.word8 0x1d,
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
okPacket x n = (0 ==) <$> (A.word8 x *> A.anyWord8 <* A.take (n - 2))

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

fromSol :: IORef Int -> TMC.TBMChan SolPacket -> Conduit SolPacket IO ByteString
fromSol watchDog upStreamChan = awaitForever $ \x -> do
    liftIO $ writeIORef watchDog 20
    case x of
        HeartBeat  n -> liftIO $ atomically $ TMC.writeTBMChan upStreamChan $ HeartBeat n
        SolData    s -> yield s
        SolControl rts dtr brk txOF loopB power rxFlTO testMode  -> liftIO $ do
            when rts   $ printInfo "SOL: RTS asserted on serial"
            when dtr   $ printInfo "SOL: DTR asserted on serial"
            when brk   $ printInfo "SOL: BRK asserted on serial"
            when power $ printInfo "SOL: power state change"
            when loopB $ printInfo "SOL: loopback mode activated"
        UserQuit -> throwM UserQuitException

toSol :: Conduit SolPacket IO ByteString
toSol = awaitForever $ \case
    HeartBeat n     -> yield $ B.pack [0x2b, 0, 0, 0, 2, 0, 0, 0]
    UserMsgToHost m -> yield $ userMsgPacket m
    UserQuit        -> throwM $ SolException "Seen ^]. Quitting app."

connectionLossQuit = await >>= \case
    Nothing -> yield UserQuit
    Just x -> yield x >> connectionLossQuit

parserToConduit p = conduitParser p =$= awaitForever (yield . snd)

hCombine :: (Source IO ByteString, Sink ByteString IO ())
         -> (Source IO ByteString, Sink ByteString IO ())
         -> IORef Int
         -> IO ()
hCombine (upSource, upSink) (downSource, downSink) watchDog = do
{-
 -  downSource -> userParser -> connectionLossQuit +-> toSol -> upSink
 -                                                 A
 -                                                 |
 - [user]                                    heartbeat chan    [AMT machine]
 -                                                 A
 -                                                 |
 -  downSink <----- solParser <---------------- fromSol <----- upSource
 -
 -}
    chan :: TMC.TBMChan SolPacket <- atomically $ TMC.newTBMChan 1

    let upSolSource = upSource =$= parserToConduit solParser =$= fromSol watchDog chan
        upSolSink   = toSol =$= upSink

        downSolSource = downSource =$= parserToConduit userParser =$= connectionLossQuit

    runResourceT $ do
        mergedDownSolSource <- TMC.mergeSources [transPipe liftIO $ TMC.sourceTBMChan chan, transPipe liftIO downSolSource] 1

        liftIO $ void $ race (mergedDownSolSource $$ upSolSink) (upSolSource $$ downSink)


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

withTimeout :: Int -> (IORef Int -> IO a) -> IO ()
withTimeout initialTimeout userF = do
    counter <- newIORef initialTimeout
    networkThread <- async (userF counter)

    f counter networkThread
    where
        f c t = poll t >>= \case
                    Nothing -> do
                        threadDelay (10^6 :: Int)
                        c' <- atomicModifyIORef' c (\x -> (x-1, x-1))
                        if c' < 0
                            then cancel t >> die "Connection timeout"
                            else f c t
                    Just (Left e) -> die $ show e
                    Just (Right _) -> exitSuccess

runAmtHandling :: ClientSettings -> String -> String -> Maybe PortNumber -> IO ()
runAmtHandling settings user pass mTcpPort = let
        hCombinator userConn = runTCPClient settings $ \server -> do
            liftIO $ printInfo "Connected to AMT host. Authenticating."
            (fromClient, ()) <- appSource server $$+ sayHello =$ appSink server
            (fromClient2, ()) <- fromClient $$++ reactPrologue user pass =$ appSink server
            liftIO $ printInfo "Authenticated. SOL active."
            (clientSource, clientFinalizer) <- CI.unwrapResumable fromClient2
            withTimeout 20 $ hCombine (clientSource, appSink server) userConn
    in case mTcpPort of
            Nothing -> withTerminalSettings $ hCombinator (CC.stdin, CC.stdout)
            Just tcpPort -> runTCPServerOnce tcpPort hCombinator

runTCPServerOnce :: PortNumber -> ((ConduitM i0 ByteString IO (), ConduitM ByteString o0 IO ()) -> IO ()) -> IO ()
runTCPServerOnce port f = do
    s <- listenOn (PortNumber port)
    putStrLn $ "Waiting for incoming TCP connection on port " ++ show port
    (h, host, clientPort) <- accept s
    putStrLn $ "connection from " ++ show host ++ ":" ++ show clientPort
    f (CC.sourceHandle h, CC.sinkHandle h) `finally` sClose s

versionString :: String
versionString = "hamtsolo " ++ $(gitHash) ++ ['+' | $(gitDirty)] ++ " (" ++ $(gitCommitDate) ++ ")"

data CliArguments = CliArguments {
    user    :: String,
    pass    :: String,
    port    :: Int,
    tcpPort :: Maybe PortNumber,
    host    :: String
}

cliArgParser :: O.Parser CliArguments
cliArgParser = CliArguments
    <$> O.option O.str   (O.short 'u' <> O.long "user" <> O.value "admin" <> O.metavar "<user>" <>
                          O.help "Authentication user name" <> O.showDefault)
    <*> O.option O.str   (O.short 'p' <> O.long "pass" <> O.value "Password123!" <> O.metavar "<password>" <>
                          O.help "Authentication password" <> O.showDefault)
    <*> O.option O.auto  (O.long "port" <> O.value 16994 <> O.metavar "<port>" <>
                          O.help "TCP connection port" <> O.showDefault)
    <*> O.optional       (O.option O.auto   ( O.long "tcppipe" <> O.metavar "<TCP pipe port>" <>
                          O.help "TCP port that shall be opened in order to route AMT SOL traffic through (instead of stdin/stdout)"))
    <*> O.argument O.str (O.metavar "<host>" <> O.help "AMT host to connect to")

main :: IO ()
main = let
    parser = O.flag' Nothing (O.long "version" <> O.hidden) <|> (Just <$> cliArgParser)
    opts   = O.info (O.helper <*> parser)
      ( O.fullDesc
      <> O.progDesc "hamtsolo lets you connect to Intel computers with enabled \
                     \AMT and establish a serial-over-lan (SOL) connection."
      <> O.header "hamtsolo - An Intel AMT Serial-Over-LAN (SOL) client" )
    in do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    mArguments <- O.execParser opts

    case mArguments of
        Nothing -> putStrLn versionString >> exitSuccess
        Just (CliArguments user pass port mTcpPort host) ->
            runAmtHandling (clientSettings port $ B2.pack host) user pass mTcpPort

