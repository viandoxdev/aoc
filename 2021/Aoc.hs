{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Aoc(
    Aoc,
    newAoc,
    getInput,
) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS   (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.UTF8 as BSU
import Data.Functor
import Control.Exception
import Text.Read
import System.IO.Error (isDoesNotExistError)
import Control.Monad

baseUrl = "https://adventofcode.com/"

getSession :: IO String
getSession = readFile "../session" <&> filter (/= '\n')

-- | Context object for Aoc related actions (fetching input, submitting answer...)
data Aoc = AocCons { session :: String, manager :: Manager }

-- | Fetch input online
fetchInput :: (Show n, Integral n) => Aoc -> n -> n -> IO String
fetchInput aoc y d = do
    let url = concat [baseUrl, show y, "/day/", show d, "/input"]
    let cookie = "session=" ++ session aoc
    initreq <- parseRequest url
    let req = initreq { requestHeaders = [("Cookie", BSU.fromString cookie)] }
    res <- httpLbs req (manager aoc)
    return $ BLU.toString $ responseBody res

-- | Fetch input online and cache it by writing it to path
fetchAndCache :: (Show n, Integral n) => Aoc -> n -> n -> String -> IO String
fetchAndCache aoc y d path = do
    input <- fetchInput aoc y d
    onException (writeFile path input) (putStrLn $ "Warning couldn't cache input to " ++ path)
    return input

-- | Get input for year and day, may download if not already cached
getInput :: (Show n, Integral n) => Aoc -> n -> n -> IO String
getInput aoc y d = do
    result <- tryJust (guard . isDoesNotExistError) (readFile path)
    case result of
        Left  _  -> fetchAndCache aoc y d path
        Right x  -> return x
    where path = "inputs/" ++ show y ++ "." ++ show d

-- | Create a new Aoc context
newAoc :: IO Aoc
newAoc = do
    session <- getSession
    manager <- newManager tlsManagerSettings
    return AocCons { session, manager }
