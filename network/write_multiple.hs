{-# LANGUAGE OverloadedStrings #-}
-- Echo client program
module Main (main) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket (AddrInfo, Socket, addrSocketType, addrAddress, addrProtocol, addrFamily, connect, socket, getAddrInfo, defaultHints, close, withSocketsDo, SocketType( Stream ))
import Network.Socket.ByteString (recv, sendAll)
import Control.Concurrent

run :: C.ByteString -> C.ByteString -> Socket -> IO C.ByteString
run a b sock
    | C.unpack a == "send" = do
        --remove the :
        let (_, content) = C.splitAt 1 b
        sendAll sock content
        return $ C.pack "sent"
    | C.unpack a == "recv" = recv sock 100
    | otherwise = return $ C.pack "unexpected command"

resolve host port = do
    let hints = defaultHints { addrSocketType = Stream }
    addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
    return addr

open :: AddrInfo -> IO Socket
open addr = do
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock $ addrAddress addr
    return sock

talk :: Socket -> Chan Msg -> IO ()
talk sock chan = do
    putStrLn "Type a command send:your_content or recv"
    line <- C.getLine
    let (cmd,content) = C.splitAt 4 line
    resp <- run cmd content sock
    C.putStrLn resp
    talk sock chan


main = do
    addr <- resolve "127.0.0.1" "3000"
    chan <- newChan        -- notice that newChan :: IO (Chan a)
    mainLoop addr chan     -- pass it into the loop
    
type Msg = C.ByteString

mainLoop :: AddrInfo -> Chan Msg -> IO ()   -- See how Chan now uses Msg.
mainLoop addr chan = do
    x <- open addr
    talk x chan
    --E.bracket (open addr) close talk
