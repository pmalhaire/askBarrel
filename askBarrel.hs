{-# LANGUAGE OverloadedStrings #-}

module Main where

-- json
import Data.Aeson
import GHC.Generics
--import Data.Text

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
import Control.Monad.Trans
import System.Console.Repline
import System.Process (callCommand)
import Data.List (isPrefixOf)
import System.Exit

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
    let addr = dbAddr ctx ++ input
    manager <- newManager defaultManagerSettings
    req addr manager ctx

read' :: IO String
read' = putStr "askBarrel> "
    >> getLine

type Repl a = HaskelineT IO a

-- Evaluation : handle each line user inputs
cmd :: String -> Repl ()
cmd input = liftIO $ print input

-- Tab Completion: return a completion for partial words entered
completer :: Monad m => WordCompleter m
completer n = do
    let names = [":quit", ":get", ":config"]
    return $ filter (isPrefixOf n) names

ctx = Context "localhost" "7080" "mydb"

-- Commands
help :: [String] -> Repl ()
help args = liftIO $ print $ "Help: " ++ show args

config :: [String] -> Repl ()
config args = liftIO $ print ctx

get :: [String] -> Repl ()
get args = do
    _ <- liftIO $ eval ctx (unwords args)
    return ()

quit :: [String] -> Repl ()
quit args = do
    _ <- liftIO $ putStrLn "Bye!"
    _ <- liftIO exitSuccess
    return ()

options :: [(String, [String] -> Repl ())]
options = [
    ("get", get)          -- :get
    , ("quit", quit)      -- :quit
    , ("help", help)      -- :help
    , ("config", config)  -- :config
    ]

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome!"

repl :: IO ()
repl = evalRepl ">>> " cmd options (Word0 completer) ini

main :: IO ()
main = repl
