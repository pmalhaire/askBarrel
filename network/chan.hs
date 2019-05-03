module Main (main) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import qualified Network.Socket as NS
import Network.Socket.ByteString (recv, sendAll)
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.Fix (fix)

-- avoid mixing :
-- putStrLn :: String -> IO()
-- C.putStrLn :: ByteString -> IO()
import Prelude hiding (putStrLn)

type Msg = C.ByteString

resolve :: String -> String -> IO NS.AddrInfo
resolve host port = do
    let hints = NS.defaultHints { NS.addrSocketType = NS.Stream }
    addr:_ <- NS.getAddrInfo
        (Just hints)
        (Just host)
        (Just port)
    return addr

open :: NS.AddrInfo -> IO NS.Socket
open addr = do
    sock <- NS.socket
        (NS.addrFamily addr)
        (NS.addrSocketType addr)
        (NS.addrProtocol addr)
    NS.connect sock $ NS.addrAddress addr
    return sock

prompt = "\x1b[0;33m>>>\x1b[0m"
back = "\r\x1b[0;32m<<<\x1b[0m"

sender :: NS.Socket -> C.ByteString -> IO ()
sender sock msg = do
    sendAll sock msg

reciever :: NS.Socket -> IO C.ByteString
reciever sock = do
    recv sock 1024


run :: NS.Socket -> IO ()
run sock = do
    chan <- newChan
    let broadcast msg = writeChan chan msg
    commLine <- dupChan chan

    forkIO $ fix $ \loop -> do
        line <- readChan commLine
        -- quit case
        if line == C.pack "quit"
            then return ()
            else do
                sender sock line
                loop

    fix $ \loop -> do
        line <- reciever sock
        if line == C.pack ""
            then return ()
            else do
                 broadcast $ C.pack back
                 broadcast line
                 broadcast $ C.pack prompt
                 loop

mainLoop :: NS.AddrInfo -> IO ()
mainLoop addr = do
    let sock = open addr
    -- handle closing
    E.bracket sock NS.close run


main :: IO()
main = do
    C.putStrLn $ C.pack "send your_content and wait for response"
    addr <- resolve "127.0.0.1" "3000"
    mainLoop addr
