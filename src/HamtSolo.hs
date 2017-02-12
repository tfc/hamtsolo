{-# LANGUAGE OverloadedStrings #-}
import           Conduit
import           Control.Concurrent.Async (concurrently)
import           Control.Monad            (void, when)
import           Data.Binary              (encode, Word8, Word16)
import           Data.ByteString          (ByteString, pack, unpack)
import qualified Data.ByteString          as B
import           Data.ByteString.Lazy     (toStrict)
import           Data.Conduit.Network
import           Data.Word8               (_cr)
import Numeric (showHex)

creds :: [(ByteString, ByteString)]
creds =
    [ ("spaceballs", "12345")
    ]

prettyPrint = concat . map (flip showHex " ") . B.unpack

auth_msg :: ByteString -> ByteString -> ByteString
auth_msg u p = let
    lu = B.length u
    lp = B.length p
    lg = 2 + lu + lp
    in
    B.concat [ pack $ map fromIntegral [0x13, 0, 0, 0, 1, lg, 0, 0, 0, lu], 
               u, pack [fromIntegral lp], p ]

start_sol_msg = let
    maxTxBuffer = 1000 :: Word16
    txBufferTimeout = 100 :: Word16
    txOverflowTimeout = 0 :: Word16
    hostSessionRxTimeout = 10000 :: Word16
    hostFifoRxFlushTimeout = 0 :: Word16
    heartbeatInterval = 5000 :: Word16
    in
    B.concat [ pack $ map fromIntegral  [0x20, 0, 0, 0, 0, 0, 0, 0],
        B.concat $ map (B.reverse . toStrict . encode)
            ([maxTxBuffer, txBufferTimeout, txOverflowTimeout, hostSessionRxTimeout, hostFifoRxFlushTimeout, heartbeatInterval] :: [Word16]),
        pack $ map fromIntegral [0, 0, 0, 0]]

checkAuth :: Conduit ByteString IO ByteString
checkAuth = do
    yield $ pack [0x10, 0x00, 0x00, 0x00, 0x53, 0x4f, 0x4c, 0x20]
    redirAw <- takeCE 13 =$= foldC
    when (B.take 2 redirAw /= pack [0x11, 0]) $ error "SOL Redirection not accepted"
    liftIO $ putStrLn "redirection accepted"

    yield $ auth_msg "admin" "Password123!"
    authAw <- takeCE 9 =$= foldC
    when (B.take 2 authAw /= pack [0x14, 0]) $ error "authentication rejected"
    liftIO $ putStrLn "authenticated"

    yield start_sol_msg
    authAw <- takeCE 23 =$= foldC
    when (B.take 2 authAw /= pack [0x21, 0]) $ error "SOL start rejected"
    liftIO $ putStrLn "SOL started"

toInt :: ByteString -> Int
toInt bs = foldr (\x y -> x + y * 0x100) 0 (map fromIntegral $ B.unpack bs)

solPipe :: Conduit ByteString IO ByteString
solPipe = do
    typ <- takeCE 1 =$= foldC
    case unpack typ of
        [0x2b] -> do -- Heartbeat from host
            garbage1 <- takeCE 3 =$= foldC
            heartbeatnum_ <- takeCE 2 =$= foldC
            let heartbeatnum = toInt heartbeatnum_ 
            liftIO $ putStrLn $ "heartbeat " ++ (show heartbeatnum)
            garbage2 <- takeCE 2 =$= foldC
            yield $ pack [0x2b, 0, 0, 0, 3, 0, 0, 0]

        [0x2a] -> do -- actual SOL data
            garbage <- takeCE 7 =$= foldC
            sizeBytes <- takeCE 2 =$= foldC
            let bytes = toInt sizeBytes
            text <- takeCE bytes =$= foldC
            liftIO $ B.putStr text
            return ()
        [0x29] -> do -- SOL control msg
            liftIO $ putStrLn "Control message"
            garbage <- takeCE 9 =$= foldC
            return ()
        [x] -> do
            liftIO $ putStrLn $ show $ fromIntegral x
            error "unknown packet number "
        [] -> error "empty input"
        x -> liftIO $ putStrLn $ "something else" ++ (show x)
    solPipe

main :: IO ()
main =
    runTCPClient (clientSettings 16994 "192.168.157") $ \server -> do
        (fromClient, ()) <- appSource server $$+ checkAuth =$ appSink server
        fromClient $$+- solPipe =$ appSink server
