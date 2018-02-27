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

handleError e addr dbConf = do
    putStr "Could not connect to "
    print addr
    putStr "Configuration is "
    print dbConf
    putStrLn ""
    print e

main :: IO ()
main = do
    let ctx = Context "localhost" "7080" "mydb"
    let addr = dbAddr ctx  ++ "docs"
    manager <- newManager defaultManagerSettings
    request <- parseRequest addr
    eres <- tryAny $ httpLbs request manager
    case eres of
       Left e -> handleError e addr ctx
       Right lbs -> readResponse lbs

