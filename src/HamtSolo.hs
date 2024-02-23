{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
import           Control.Applicative            ( (<|>) )
import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.Async
import           Control.Exception              ( Exception
                                                , bracket
                                                , try
                                                )
import           Control.Monad                  ( unless
                                                , when
                                                )
import           Control.Monad.Catch            ( throwM )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Trans.Resource   ( runResourceT )
import           Data.Attoparsec.Binary         ( anyWord16le )
import qualified Data.Attoparsec.ByteString    as A
import           Data.Binary                    ( Word16
                                                , Word8
                                                , encode
                                                )
import           Data.Bits                      ( testBit )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as B2
import           Data.ByteString.Lazy           ( toStrict )
import           Data.Conduit
import qualified Data.Conduit                  as CI
import           Data.Conduit.Attoparsec        ( conduitParser
                                                , sinkParser
                                                )
import qualified Data.Conduit.Combinators      as CC
import           Data.Conduit.Network
import qualified Data.Conduit.TMChan           as TMC
import           Data.Functor                   ( ($>) )
import           Data.IORef
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                )
import           Data.Monoid                    ( (<>) )
import           Data.Typeable
import           Development.GitRev
import           GHC.IO.Exception               ( IOException )
import qualified Options.Applicative           as O
import           System.Exit                    ( die
                                                , exitSuccess
                                                )
import           System.IO                      ( BufferMode(..)
                                                , hPutStrLn
                                                , hSetBuffering
                                                , stderr
                                                , stdin
                                                , stdout
                                                )
import           System.Posix.IO                ( stdInput )
import qualified System.Posix.Terminal         as T

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
solParser = A.choice
  [ do
    A.word8 0x2b
    A.take 3
    n <- anyWord16le
    A.take 2
    return $ HeartBeat $ fromIntegral n
  , do
    A.word8 0x2a
    A.take 7
    n <- anyWord16le
    s <- A.take $ fromIntegral n
    return $ SolData s
  , do
    A.word8 0x29
    A.take 7
    control <- A.anyWord8
    status  <- A.anyWord8
    return $ SolControl (testBit control 0)
                        (testBit control 1)
                        (testBit control 2)
                        (testBit status 0)
                        (testBit status 1)
                        (testBit status 2)
                        (testBit status 3)
                        (testBit status 4)
  ]

userParser :: A.Parser SolPacket
userParser = A.choice
  [A.word8 0x1d $> UserQuit, UserMsgToHost . B.singleton <$> A.anyWord8]

authMsg :: ByteString -> ByteString -> ByteString
authMsg u p =
  let lu = fromIntegral (B.length u) :: Word8
      lp = fromIntegral (B.length p) :: Word8
      lg = 2 + lu + lp
  in  B.concat [B.pack [0x13, 0, 0, 0, 1, lg, 0, 0, 0, lu], u, B.pack [lp], p]

startSolMsg :: ByteString
startSolMsg =
  let maxTxBuffer            = 1000 :: Word16
      txBufferTimeout        = 100 :: Word16
      txOverflowTimeout      = 0 :: Word16
      hostSessionRxTimeout   = 10000 :: Word16
      hostFifoRxFlushTimeout = 0 :: Word16
      heartbeatInterval      = 5000 :: Word16
  in  B.concat
        [ B.pack [0x20, 0, 0, 0, 0, 0, 0, 0]
        , B.concat $ map
          (B.reverse . toStrict . encode)
          [ maxTxBuffer
          , txBufferTimeout
          , txOverflowTimeout
          , hostSessionRxTimeout
          , hostFifoRxFlushTimeout
          , heartbeatInterval
          ]
        , B.pack [0, 0, 0, 0]
        ]

sayHello :: ConduitT ByteString ByteString IO ()
sayHello = yield $ B.pack [0x10, 0x00, 0x00, 0x00, 0x53, 0x4f, 0x4c, 0x20]

okPacket :: Word8 -> Int -> A.Parser Bool
okPacket x n = do
  A.word8 x
  bad <- A.anyWord8
  A.take (n - 2)
  return $ bad == 0

userMsgPacket :: ByteString -> ByteString
-- TODO: send word16 with actual length instead of [1, 0]
userMsgPacket bs =
  let patchedMsg = B.map (\c -> if c == 0xa then 0xd else c) bs -- transform LF to CR
  in  B.concat [B.pack [0x28, 0, 0, 0, 0, 0, 0, 0], B.pack [1, 0], patchedMsg]

acceptPacketOrThrow
  :: String -> A.Parser Bool -> ConduitT ByteString ByteString IO ()
acceptPacketOrThrow errStr p = do
  packetGood <- sinkParser p
  unless packetGood $ throwM $ SolException errStr

reactPrologue :: String -> String -> ConduitT ByteString ByteString IO ()
reactPrologue user pass = do
  acceptPacketOrThrow "Server does not accept redirection request."
    $ okPacket 0x11 13
  yield $ authMsg (B2.pack user) (B2.pack pass)

  acceptPacketOrThrow "Server does not accept authentication." $ okPacket 0x14 9
  yield startSolMsg

  acceptPacketOrThrow "Authenticated, but Server refuses SOL."
    $ okPacket 0x21 23

printInfo :: String -> IO ()
printInfo = hPutStrLn stderr

reactSolMode :: (IORef Int, Int) -> ConduitT SolPacket ByteString IO ()
reactSolMode (counter, timeout) = awaitForever $ \x -> do
  liftIO $ writeIORef counter timeout
  case x of
    HeartBeat n -> yield $ B.pack [0x2b, 0, 0, 0, 2, 0, 0, 0]
    SolData   s -> liftIO $ B.putStr s
    SolControl rts dtr brk txOF loopB power rxFlTO testMode -> liftIO $ do
      when rts $ printInfo "SOL: RTS asserted on serial"
      when dtr $ printInfo "SOL: DTR asserted on serial"
      when brk $ printInfo "SOL: BRK asserted on serial"
      when power $ printInfo "SOL: power state change"
      when loopB $ printInfo "SOL: loopback mode activated"
    UserQuit        -> throwM UserQuitException
    UserMsgToHost m -> yield $ userMsgPacket m

withTerminalSettings :: IO r -> IO r
withTerminalSettings runStuff =
  let setStdinAttrs a = T.setTerminalAttributes stdInput a T.WhenFlushed
  in
    bracket
      (do
        eOldSettings :: Either IOException T.TerminalAttributes <- try
          $ T.getTerminalAttributes stdInput
        case eOldSettings of
          Left  _           -> return Nothing
          Right oldSettings -> do
            let newSettings =
                  flip T.withMinInput 1
                    $   flip T.withTime 0
                    $   foldr (flip T.withoutMode) oldSettings
                    [T.KeyboardInterrupts, T.EnableEcho, T.ProcessInput]

            setStdinAttrs newSettings
            return $ Just oldSettings
      )
      (mapM_ setStdinAttrs)
      (const runStuff)

withTimeout :: Int -> ((IORef Int, Int) -> IO a) -> IO ()
withTimeout timeout userF = do
  counter       <- newIORef timeout
  networkThread <- async (userF (counter, timeout))

  f counter networkThread
 where
  f c t = poll t >>= \case
    Nothing -> do
      threadDelay (10 ^ 6 :: Int)
      c' <- atomicModifyIORef' c (\x -> (x - 1, x - 1))
      if c' < 0 then cancel t >> die "Connection timeout" else f c t
    Just (Left  e) -> die $ show e
    Just (Right _) -> exitSuccess

runAmtHandling
  :: ClientSettings -> String -> String -> (IORef Int, Int) -> IO ()
runAmtHandling settings user pass watchDog =
  runTCPClient settings $ \server -> do
    liftIO $ printInfo "Connected. Authenticating."
    (fromClient, ()) <- appSource server $$+ sayHello .| appSink server
    liftIO $ printInfo "Authenticated. SOL active."
    withTerminalSettings $ do
      (fromClient2 :: CI.SealedConduitT () ByteString IO (), _) <-
        fromClient $$++ (reactPrologue user pass .| appSink server)
      let clientSource = CI.unsealConduitT fromClient2

      let sckIn = transPipe liftIO (clientSource .| conduitParser solParser)
      let kbdIn        = transPipe liftIO (CC.stdin .| conduitParser userParser)

      runResourceT $ do
        sources <- TMC.mergeSources [sckIn, kbdIn] 2
        runConduit $ sources .| awaitForever (yield . snd) .| transPipe
          liftIO
          (reactSolMode watchDog .| appSink server)

versionString :: String
versionString =
  "hamtsolo "
    ++ $(gitHash)
    ++ [ '+' | $(gitDirty) ]
    ++ " ("
    ++ $(gitCommitDate)
    ++ ")"

data CliArguments = CliArguments
  { user    :: String
  , pass    :: String
  , port    :: Int
  , host    :: String
  , timeout :: Int
  }

cliArgParser :: O.Parser CliArguments
cliArgParser =
  CliArguments
    <$> O.option
          O.str
          (  O.short 'u'
          <> O.long "user"
          <> O.value "admin"
          <> O.metavar "<user>"
          <> O.help "Authentication user name"
          <> O.showDefault
          )
    <*> O.option
          O.str
          (  O.short 'p'
          <> O.long "pass"
          <> O.value "Password123!"
          <> O.metavar "<password>"
          <> O.help "Authentication password"
          <> O.showDefault
          )
    <*> O.option
          O.auto
          (  O.long "port"
          <> O.value 16994
          <> O.metavar "<port>"
          <> O.help "TCP connection port"
          <> O.showDefault
          )
    <*> O.argument O.str (O.metavar "<host>" <> O.help "AMT host to connect to")
    <*> O.option
          O.auto
          (  O.short 't'
          <> O.long "timeout"
          <> O.value 60
          <> O.metavar "<timeout>"
          <> O.help "Timeout in seconds"
          <> O.showDefault
          )

main :: IO ()
main =
  let
    parser =
      O.flag' Nothing (O.long "version" <> O.hidden) <|> (Just <$> cliArgParser)
    opts = O.info
      (O.helper <*> parser)
      (  O.fullDesc
      <> O.progDesc
           "hamtsolo lets you connect to Intel computers with enabled \
                     \AMT and establish a serial-over-lan (SOL) connection."
      <> O.header "hamtsolo - An Intel AMT Serial-Over-LAN (SOL) client"
      )
  in
    do
      hSetBuffering stdin  NoBuffering
      hSetBuffering stdout NoBuffering
      mArguments <- O.execParser opts

      case mArguments of
        Nothing -> putStrLn versionString >> exitSuccess
        Just (CliArguments user pass port host timeout) ->
          withTimeout timeout
            $ runAmtHandling (clientSettings port $ B2.pack host) user pass
