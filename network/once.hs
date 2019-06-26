{-# LANGUAGE OverloadedStrings #-}
-- Echo client program
module Main (main) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket as NS
import Network.Socket.ByteString (recv, sendAll)

-- resolve server address info
resolve :: String -> String -> IO NS.AddrInfo
resolve host port = do
    let hints = NS.defaultHints { NS.addrSocketType = NS.Stream }
    addr:_ <- NS.getAddrInfo
        (Just hints)
        (Just host)
        (Just port)
    return addr

-- open socket from addr info
open :: NS.AddrInfo -> IO NS.Socket
open addr = do
    sock <- NS.socket
        (NS.addrFamily addr)
        (NS.addrSocketType addr)
        (NS.addrProtocol addr)
    NS.connect sock $ NS.addrAddress addr
    return sock

-- talk to the server tell hello world
talk sock = do
    sendAll sock "Hello, world!"

-- send data to the server
main :: IO ()
main = withSocketsDo $ do
    -- resolve address
    addr <- resolve "127.0.0.1" "3000"
    -- send our data with error handling
    E.bracket (open addr) close talk
