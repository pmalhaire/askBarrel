{-# LANGUAGE OverloadedStrings #-}

module Main where

-- json
import Data.Aeson
import GHC.Generics()

-- pretty print
import qualified Data.ByteString.Lazy.Char8 as C (unpack)
import qualified Data.ByteString.Lazy.Internal as I (ByteString)
import Data.Aeson.Encode.Pretty

-- http
import Network.HTTP.Client (newManager, Manager,defaultManagerSettings,
    responseStatus, responseBody, httpLbs, parseRequest)
import Network.HTTP.Types.Status (statusCode)
import Control.Exception.Enclosed

-- REPL
import Control.Monad.Trans
import System.Console.Repline
import Data.List (isPrefixOf)
import System.Exit
import System.Environment

setConfig :: [String] -> IO ()
-- default config
setConfig a = do
                setEnv "ASK_BARREL_HOST" host
                setEnv "ASK_BARREL_PORT" port
                setEnv "ASK_BARREL_DB" db
                putStrLn $ "http://" ++ host ++ ":" ++ port ++ "/dbs/" ++ db
              where
                (host, port, db) = if null a
                    then ("localhost", "7080", "mydb")
                    else (last a, head a, head $ tail a)

dbAddr :: IO String
dbAddr = do
        host   <- getEnv "ASK_BARREL_HOST"
        port   <- getEnv "ASK_BARREL_PORT"
        db     <- getEnv "ASK_BARREL_DB"
        return $ "http://" ++ host ++ ":" ++ port ++ "/dbs/" ++ db ++ "/"

readDbAddr :: IO ()
readDbAddr = dbAddr >>= putStrLn

prettyPrint :: I.ByteString -> String
prettyPrint "" = "empty json"
prettyPrint a = C.unpack $ encodePretty ( decode a :: Maybe Value )

readResponse resp = do
    putStrLn $ "Status: " ++ show status
    putStrLn $ prettyPrint body
    where
        status = statusCode $ responseStatus resp
        body = responseBody resp

handleError :: (Show a2, Show a1) => a2 -> a1 -> IO ()
handleError e addr = do
    putStr "Could not connect to "
    print addr
    putStrLn ""
    print e

getRequest :: String -> Manager -> IO ()
getRequest input manager = do
    db <- dbAddr
    let addr = db ++ input
    request <- parseRequest addr
    eres <- tryAny $ httpLbs request manager
    case eres of
       Left e -> handleError e addr
       Right lbs -> readResponse lbs

--todo use monad
get :: String -> IO ()
get input = do
    manager <- newManager defaultManagerSettings
    getRequest input manager

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
    let names = [":quit", ":doc", ":docs", ":config"]
    return $ filter (isPrefixOf n) names

-- Commands

--TODO fill help
help :: [String] -> Repl ()
help args = liftIO $ print $ "Help: " ++ show args

config :: [String] -> Repl ()
config _ = do
    _ <- liftIO readDbAddr
    return ()

docs :: [String] -> Repl ()
docs _ = do
    _ <- liftIO $ get "docs"
    return ()

doc :: [String] -> Repl ()
doc args = do
    let docUrl = "docs/" ++ unwords args
    _ <- liftIO $ get docUrl
    return ()

quit :: [String] -> Repl ()
quit _ = do
    _ <- liftIO $ putStrLn "Bye!"
    _ <- liftIO exitSuccess
    return ()

options :: [(String, [String] -> Repl ())]
options = [
    ("doc", doc)          -- :doc
    , ("docs", docs)      -- :docs
    , ("quit", quit)      -- :quit
    , ("help", help)      -- :help
    , ("config", config)  -- :config
    ]

ini :: Repl ()
ini = do liftIO $ putStrLn "Welcome to ask Barrel!\n"
         liftIO $ putStrLn "   .- ¨¨¨¨ -.   "
         liftIO $ putStrLn "  /'-.____.-'\\"
         liftIO $ putStrLn "  '-.______.-'  "
         liftIO $ putStrLn " |    \\ \\     |"
         liftIO $ putStrLn " |    /  \\    |"
         liftIO $ putStrLn "     / /\\ \\    "
         liftIO $ putStrLn "  \\'-.____.-'/"
         liftIO $ putStrLn "   '-.____.-'   "

repl :: IO ()
repl = evalRepl ">>> " cmd options (Word0 completer) ini

main :: IO ()
main = do setConfig =<< getArgs -- set db config
          repl
