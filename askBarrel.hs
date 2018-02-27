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

data HostConfig = HostConfig {
    host :: String,
    port :: String
}

instance Show HostConfig where
    show (HostConfig host port) = host ++ ":" ++ port

data DbConfig = DbConfig {
    hostConf :: HostConfig,
    db :: String
}

instance Show DbConfig where
    show (DbConfig hostConf db) = host hostConf ++ ":" ++ port hostConf ++ " " ++ db

-- TODO create connection context
-- TODO split HTTP and barrel code

dbAddr :: DbConfig -> String
dbAddr conf = "http://" ++ host (hostConf conf) ++ ":" ++ port (hostConf conf)
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
    let hostConf = HostConfig "localhost" "7080"
    let dbConf = DbConfig hostConf "mydb"
    let addr = dbAddr dbConf  ++ "docs"
    manager <- newManager defaultManagerSettings
    request <- parseRequest addr
    eres <- tryAny $ httpLbs request manager
    case eres of
       Left e -> handleError e addr dbConf
       Right lbs -> readResponse lbs

