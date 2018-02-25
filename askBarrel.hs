{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module BarrelClient where

import Data.Aeson
import GHC.Generics
import Data.Text

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
