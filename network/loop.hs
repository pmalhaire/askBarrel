{-# LANGUAGE OverloadedStrings #-}
-- Echo client program
module Main (main) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket (addrSocketType, addrAddress, addrProtocol, addrFamily, connect, socket, getAddrInfo, defaultHints, close, withSocketsDo, SocketType( Stream ))
import Network.Socket.ByteString (recv, sendAll)



main :: IO ()
main = withSocketsDo $ do
    addr <- resolve "127.0.0.1" "3000"
    E.bracket (open addr) close talk
  where
    resolve host port = do
        let hints = defaultHints { addrSocketType = Stream }
        addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
        return addr
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock $ addrAddress addr
        return sock
    talk sock = do
        x <- C.getLine
        sendAll sock x
        y <- recv sock 100
        C.putStrLn y
        talk sock
