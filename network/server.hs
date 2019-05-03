module Main (main) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import qualified Network.Socket as NS
import Network.Socket.ByteString (recv, sendAll)
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.Fix (fix)
import Control.Monad (forever, void)

-- avoid mixing :
-- putStrLn :: String -> IO()
-- C.putStrLn :: ByteString -> IO()
import Prelude hiding (putStrLn)

type Msg = C.ByteString


-- symbols
back    = "\x1b[0;33m>>>\x1b[0m"
input   = "\x1b[0;32m<<<\x1b[0m"
close   = "\x1b[0;94mx-x\x1b[0m"

connect = "\x1b[0;36m-<>-\x1b[0m"
comm    = "\x1b[0;36m-||-\x1b[0m"
end     = "\x1b[0;36m-><-\x1b[0m"

sender :: NS.Socket -> C.ByteString -> IO ()
sender sock msg = do
    sendAll sock msg

reciever :: NS.Socket -> IO C.ByteString
reciever sock = do
    recv sock 1024


run :: NS.Socket -> IO ()
run sock =  forever $ do
    (conn, peer) <- NS.accept sock
    let name = show peer
    C.putStrLn $ C.pack $ connect ++ "connection from " ++ name
    void $ forkFinally (talk conn name) (\_ -> NS.close conn)

talk conn name = do
    chan <- newChan
    let broadcast msg = writeChan chan msg
    commLine <- dupChan chan

    -- fork off a thread for reading from the duplicated channel
    forkIO $ fix $ \loop -> do
        line <- readChan commLine
        -- quit case
        if line == C.pack "quit"
            then return ()
            else do
                C.putStrLn $ C.concat [(C.pack comm), (C.pack name),(C.pack back),line]
                sender conn line
                loop

    -- read lines from the socket and echo them back to the user
    fix $ \loop -> do
        line <- reciever conn
        if line == C.pack ""
            then do
                C.putStrLn $ C.concat [(C.pack end), (C.pack name), (C.pack close), (C.pack "connection closed")]
                return ()
            else do
                 C.putStrLn $ C.concat [(C.pack comm), (C.pack name),(C.pack input),line]
                 broadcast $ C.append (C.pack "recieved:") line
                 loop

resolve :: String -> IO NS.AddrInfo
resolve port = do
    let hints = NS.defaultHints {
            NS.addrFlags = [NS.AI_PASSIVE]
            , NS.addrSocketType = NS.Stream
            }
    addr:_ <- NS.getAddrInfo (Just hints) Nothing (Just port)
    return addr

open :: String -> IO NS.Socket
open port = do
            sock <- NS.socket NS.AF_INET NS.Stream 0
            addr <- resolve port
            NS.setSocketOption sock NS.ReuseAddr 1
            NS.bind sock (NS.addrAddress addr)
            NS.listen sock 10
            return sock

mainLoop :: String -> IO ()
mainLoop port = do
    let sock = open port
    -- handle closing
    E.bracket sock NS.close run


main :: IO()
main = do
    C.putStrLn $ C.pack "server ready for connections"
    mainLoop "3000"
