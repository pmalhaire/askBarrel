module Main (main) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import qualified Network.Socket as NS
import Network.Socket.ByteString (recv, sendAll)
import Control.Concurrent
import Control.Monad.Fix (fix)
import Control.Concurrent.Async

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

push :: NS.Socket -> C.ByteString -> IO ()
push sock content = do
    -- dropping the result may not be good
    x <- sendAll sock content
    return x

prompt = "\x1b[0;33m>>>\x1b[0m"
back = "\r\x1b[0;32m<<<\x1b[0m"

sender :: NS.Socket -> C.ByteString -> IO ()
sender sock msg = do
    push sock msg

get :: NS.Socket -> IO C.ByteString
get sock = do
    resp <- recv sock 1024
    return resp

reciever :: NS.Socket -> IO C.ByteString
reciever sock = do
    resp <- get sock
    return resp

run :: NS.Socket -> IO ()
run sock = do
    chan <- newChan
    let broadcast msg = writeChan chan msg
    commLine <- dupChan chan
    -- send
    sendThread <- async $ fix $ \loop -> do
        C.putStr $ C.pack prompt
        line <- C.getLine
        if line == C.pack "quit"
            then do
                return ()
            else do
                sender sock line
                loop
    -- recieve
    recieveThread <- async $ fix $ \loop -> do
        line <- reciever sock
        if line == C.pack ""
            then return ()
            else do
                 C.putStr $ C.pack back
                 C.putStrLn line
                 C.putStr $ C.pack prompt
                 loop

    wait sendThread
    cancel recieveThread


mainLoop :: NS.AddrInfo -> IO ()
mainLoop addr = do
    let sock = open addr
    -- send data asyncronously
    E.bracket sock NS.close run


main :: IO()
main = do
    C.putStrLn $ C.pack "send your_content and wait for response"
    addr <- resolve "127.0.0.1" "3000"
    mainLoop addr
