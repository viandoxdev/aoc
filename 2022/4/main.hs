{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Client
import Network.HTTP.Client.TLS   (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.UTF8 as BSU
import Data.List.Split

parseLine :: (Num n, Read n) => String -> ((n, n),(n, n))
parseLine line = let [a, b, c, d] = map read (splitOneOf ",-" line) in ((a, b), (c, d))

(<?) :: Ord n => n -> (n, n) -> Bool
(<?) x (a, b) = a <= x && x <= b

overlapsFully :: Ord n => (n, n) -> (n, n) -> Bool
overlapsFully (a, b) (c, d) = let (ab, cd) = ((a,b),(c,d)) in (c <? ab && d <? ab) || (a <? cd && b <? cd)

overlaps :: Ord n => (n, n) -> (n, n) -> Bool
overlaps (a, b) (c, d) = let (ab, cd) = ((a,b),(c,d)) in c <? ab || d <? ab || a <? cd || b <? cd

main :: IO ()
main = do
    -- read session
    sessionFile <- readFile "../../session"
    let session = filter (/= '\n') sessionFile

    -- prepare request
    manager <- newManager tlsManagerSettings

    initRequest <- parseRequest "https://adventofcode.com/2022/day/4/input"
    let request = initRequest { requestHeaders = [("Cookie", BSU.fromString $ "session=" ++ session)] }
    
    -- send request
    response <- httpLbs request manager

    let input = map parseLine (lines $ BLU.toString $ responseBody response)
    let result1 = length (filter (uncurry overlapsFully) input)
    let result2 = length (filter (uncurry overlaps) input)
    putStrLn $ "part 1: " ++ show result1
    putStrLn $ "part 2: " ++ show result2
