{-# LANGUAGE TupleSections #-}
module Challenges.Day12 where
import Data.Array
import Utils
import Control.Monad ((<=<))
import Data.List (elemIndex, intercalate, sort, group, (\\))
import Data.Maybe (fromJust)

type Map a = Array Int a
data Size = Big | Small deriving (Eq)

mapNames :: String -> [String]
mapNames = (["start", "end"]++) . (\\ ["start", "end"]) . map head . group . sort . (splitOn "-" <=< lines)
            -- indexed of start and end are set to 0 and 1

name :: String -> [String] -> Int
name n = fromJust . elemIndex n

connections :: String -> [String] -> Map [Int]
connections str names = accumArray (++) [] (0, length names - 1) $ concatMap (f . splitOn "-") $ lines str
    where f [l, r] = [(n l, [n r]), (n r, [n l])] where n = (`name` names)
          f _ = error "Bad input"

sizes :: [String] -> Map Size
sizes names = array (0, length names - 1) $ zip [0..] $ map size names where
    size s = if all (`elem` ['A'..'Z']) s then Big else Small

parse :: String -> (Map [Int], Map Size)
parse str = (connections str names, sizes names)
    where names = mapNames str

solve :: (Map Size -> Map Int -> Bool -> [Int] -> [(Bool, Int)]) -> (Map [Int], Map Size, Map Int) -> Bool -> Int -> Int
solve tsfm (_, _, _) _ 1 = 1 -- index 1 is end
solve tsfm (con, sz, ps) us cur = sum . map (uncurry $ solve tsfm (con, sz, nps)) $ tsfm sz ps us $ con!cur
    where nps = if sz!cur == Small then accum (+) ps [(cur, 1)] else ps

part1 :: (Map [Int], Map Size) -> Int
part1 (con, sz) = solve tsfm (con, sz, accumArray (+) 0 (bounds con) []) True 0 -- index 0 is start
    where tsfm sz ps _ = map (True,) . filter (\e -> sz!e == Big || ps!e < 1)

part2 :: (Map [Int], Map Size) -> Int
part2 (con, sz) = solve tsfm (con, sz, accumArray (+) 0 (bounds con) []) False 0 where
    tsfm sz ps True = map (True,) . filter (\e -> sz!e == Big || ps!e < 1)
    tsfm sz ps False = map (\e -> (not (sz!e == Big || ps!e < 1), e)) . filter (/=0)

day12 :: String -> (String, String)
day12 str = (show $ part1 inp, show $ part2 inp) where inp = parse str
