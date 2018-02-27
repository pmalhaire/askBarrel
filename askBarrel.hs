--{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module BarrelClient where

import Data.Aeson
import GHC.Generics
import Data.Text

-- for pretty print
import qualified Data.ByteString.Lazy.Char8 as C (unpack)
import qualified Data.ByteString.Lazy.Internal as I (ByteString)
import Data.Aeson.Encode.Pretty


import Network.HTTP.Client (newManager, defaultManagerSettings,
    responseStatus, responseBody, httpLbs, parseRequest)
import Network.HTTP.Types.Status (statusCode)
import Control.Exception.Enclosed
{-|
get :: String -> String
get []  = error "emtpy get is invalid"
get str = "GET " ++ str

put :: String -> String
put [] = error "empty put is invalid"
put str = "PUT " ++ str


path :: String -> String -> String
path  "" doc = error "empty db"
path    db "" = error "empty doc"
path db doc = "/" ++ db ++ "/docs/" ++ doc


getPath :: String -> String -> String
getPath db doc = get (path db doc)

putPath :: String -> String -> String -> String
putPath db doc content = put ( path db doc ) ++ content

data Doc = Doc {
      docId    :: Text
    , value  :: Value
    } deriving (Generic, Show)

instance FromJSON Doc where
    parseJSON (Object v) =
        Doc <$> v .: "id"
            <*> v .: "value"


instance ToJSON Doc where
    toJSON (Doc docId value) =
        object [ "id"      .= docId
                , "value"   .= value
                ]
-}
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

dbAddr :: DbConfig -> String
dbAddr conf = "http://" ++ host (hostConf conf) ++ ":" ++ port (hostConf conf)
  ++ "/dbs/" ++ db conf ++ "/"

prettyPrint :: I.ByteString -> String
prettyPrint "" = "empty json"
prettyPrint a = C.unpack $ encodePretty ( decode a :: Maybe Value )

readResponse resp =
    putStrLn $ "The status code was: " ++ show status ++ prettyPrint body
    where
        status = statusCode $ responseStatus resp
        body = responseBody resp

handleError e addr dbConf = do
    putStr "Could not connect to "
    print addr
    putStr "Configuration is "
    print dbConf
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

