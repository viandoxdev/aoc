module Challenges.Day01 (day01) where

import Utils (windows)

parse :: String -> [Int]
parse input = map read $ lines input

part1 :: [Int] -> Int
part1 l = fst $ foldl f (0, head l) l where
    f (t, xs) i 
        | i > xs = (t + 1, i)
        | otherwise = (t, i)

part2 :: [Int] -> Int
part2 = part1 . map sum . windows 3

day01 :: String -> (String, String)
day01 str = (show $ part1 input, show $ part2 input)
    where input = parse str
