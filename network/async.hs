module Main (main) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import qualified Network.Socket as NS
import Network.Socket.ByteString (recv, sendAll)
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

sender :: NS.Socket -> IO ()
sender sock = do
    -- prompt string
    C.putStr $ C.pack prompt
    -- get string from command line
    content <- C.getLine
    if content == C.pack "quit"
        then return ()
        else do
            push sock content
            sender sock

get :: NS.Socket -> IO C.ByteString
get sock = do
    resp <- recv sock 1024
    return resp

reciever :: NS.Socket -> IO ()
reciever sock = do
    resp <- get sock
    -- response on other line
    C.putStr $ C.pack back
    C.putStrLn resp
    C.putStr $ C.pack prompt
    if resp == C.pack ""
        then return ()
        else reciever sock

run :: NS.Socket -> IO ()
run sock = do
    async(reciever sock)
    sender sock


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
