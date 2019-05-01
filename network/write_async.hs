{-# LANGUAGE OverloadedStrings #-}
-- Echo client program
module Main (main) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket (AddrInfo, Socket, addrSocketType, addrAddress, addrProtocol, addrFamily, connect, socket, getAddrInfo, defaultHints, close, withSocketsDo, SocketType( Stream ))
import Network.Socket.ByteString (recv, sendAll)
import Control.Concurrent

resolve host port = do
    let hints = defaultHints { addrSocketType = Stream }
    addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
    return addr

open :: AddrInfo -> IO Socket
open addr = do
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock $ addrAddress addr
    return sock

talk :: Socket -> IO ()
talk sock = do
    putStrLn "send your_content and wait for response"
    content <- C.getLine
    ack <- run sock content
    C.putStrLn ack
    talk sock

run :: Socket -> C.ByteString -> IO C.ByteString
run sock content = do
    _ <- sendAll sock content
    forkIO( do
        resp <- recv sock 100
        C.putStrLn resp )
    return $ C.pack "sent"
    
main = do
    addr <- resolve "127.0.0.1" "3000"
    chan <- newChan        -- notice that newChan :: IO (Chan a)
    mainLoop addr chan     -- pass it into the loop
    
type Msg = C.ByteString

mainLoop :: AddrInfo -> Chan Msg -> IO ()   -- See how Chan now uses Msg.
mainLoop addr chan = do
    E.bracket (open addr) close talk
