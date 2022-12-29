module Challenges.Day01 where

import Utils (windows)

parse :: (Integral n, Read n) => String -> [n]
parse input = map read $ lines input

part1 :: Integral n => [n] -> n
part1 l = fst $ foldl f (0, head l) l where
    f (t, l) i 
        | i > l = (t + 1, i)
        | otherwise = (t, i)

part2 :: Integral n => [n] -> n
part2 = part1 . map sum . windows 3

day01 :: String -> (String, String)
day01 str = (show $ part1 input, show $ part2 input)
    where input = parse str
