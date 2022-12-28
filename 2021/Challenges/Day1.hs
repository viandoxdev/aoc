module Challenges.Day1 where

import Aoc
import Control.Applicative (ZipList(getZipList, ZipList))
import Data.List (tails)

parse :: (Integral n, Read n) => String -> [n]
parse input = map read $ lines input

transpose' :: [[a]] -> [[a]]
transpose' = getZipList . traverse ZipList

windows :: Int -> [a] -> [[a]]
windows n = transpose' . take n . tails

part1 :: Integral n => [n] -> n
part1 l = fst $ foldl f (0, head l) l where
    f (t, l) i 
        | i > l = (t + 1, i)
        | otherwise = (t, i)

part2 :: Integral n => [n] -> n
part2 = part1 . map sum . windows 3

day1 :: String -> (String, String)
day1 str = (show $ part1 input, show $ part2 input)
    where input = parse str
