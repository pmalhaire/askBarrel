module Main (main) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import qualified Network.Socket as NS
import Network.Socket.ByteString (recv, sendAll)
import Control.Concurrent

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

run :: NS.Socket -> C.ByteString -> IO C.ByteString
run sock content = do
    _ <- sendAll sock content
    -- wait for response asyncronously
    forkIO( do
        resp <- recv sock 100
        C.putStrLn resp )
    return $ C.pack "sent"

talk :: NS.Socket -> IO ()
talk sock = do
    -- prompt string
    C.putStr $ C.pack ">>>"
    -- get string from command line
    content <- C.getLine
    -- send data
    ack <- run sock content
    -- show ack
    C.putStrLn ack

    talk sock

mainLoop :: NS.AddrInfo -> IO ()
mainLoop addr = do
    E.bracket (open addr) NS.close talk

main :: IO()
main = do
    C.putStrLn $ C.pack "send your_content and wait for response"
    addr <- resolve "127.0.0.1" "3000"
    mainLoop addr
