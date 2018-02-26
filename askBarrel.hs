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
} deriving (Show)

dbAddr :: HostConfig -> String -> String
dbAddr conf db = "http://" ++ host conf ++ ":" ++ port conf
  ++ "/dbs/" ++ db ++ "/"

prettyPrint :: I.ByteString -> IO ()
prettyPrint "" = putStrLn "empty json"
prettyPrint a = putStrLn $ C.unpack $ encodePretty ( decode a :: Maybe Value )

main :: IO ()
main = do
    let config = HostConfig "localhost" "7080"
    let addr = dbAddr config "mydb" ++ "docs"
    manager <- newManager defaultManagerSettings
    request <- parseRequest addr
    response <- httpLbs request manager
    let status = statusCode $ responseStatus response
    putStrLn $ "The status code was: " ++ show status
    let body = responseBody response
    prettyPrint body
