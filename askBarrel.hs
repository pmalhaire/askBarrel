{-# LANGUAGE OverloadedStrings #-}

module BarrelClient where

-- json
import Data.Aeson
import GHC.Generics
import Data.Text

-- pretty print
import qualified Data.ByteString.Lazy.Char8 as C (unpack)
import qualified Data.ByteString.Lazy.Internal as I (ByteString)
import Data.Aeson.Encode.Pretty

-- http
import Network.HTTP.Client (newManager, defaultManagerSettings,
    responseStatus, responseBody, httpLbs, parseRequest)
import Network.HTTP.Types.Status (statusCode)
import Control.Exception.Enclosed

-- REPL
import Control.Monad (unless)
import System.IO

data Context = Context {
    host :: String,
    port :: String,
    db :: String
}

instance Show Context where
    show (Context host port db) = host ++ ":" ++ port ++ " " ++ db

-- TODO create connection context
-- TODO split HTTP and barrel code

dbAddr :: Context -> String
dbAddr conf = "http://" ++ host conf ++ ":" ++ port conf
  ++ "/dbs/" ++ db conf ++ "/"

prettyPrint :: I.ByteString -> String
prettyPrint "" = "empty json"
prettyPrint a = C.unpack $ encodePretty ( decode a :: Maybe Value )

readResponse resp = do
    putStrLn $ "Status: " ++ show status
    putStrLn $ prettyPrint body
    where
        status = statusCode $ responseStatus resp
        body = responseBody resp

-- todo find exception type
handleError e addr ctx = do
    putStr "Could not connect to "
    print addr
    putStr "Configuration is "
    print ctx
    putStrLn ""
    print e

req addr manager ctx = do
    request <- parseRequest addr
    eres <- tryAny $ httpLbs request manager
    case eres of
       Left e -> handleError e addr ctx
       Right lbs -> readResponse lbs

eval ctx input = do
    let addr = dbAddr ctx  ++ input
    manager <- newManager defaultManagerSettings
    req addr manager ctx

read' :: IO String
read' = putStr "askBarrel> "
    >> getLine

main :: IO ()
main = do
    let ctx = Context "localhost" "7080" "mydb"
    input <- read'
    unless (input == ":quit") $ eval ctx input
        >> main
