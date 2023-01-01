module Challenges.Day07 where
import Utils (splitOn, median)
import Data.List (sort, minimumBy)

parse :: String -> [Int]
parse = map read . splitOn ","

avg :: [Int] -> Int
avg = uncurry div . foldl f (0, 0) where
    f (t, l) i = (t + i, l + 1)

dist :: Int -> Int -> Int
dist f t = abs (f - t)

dist' :: Int -> Int -> Int
dist' f t = (d * (d + 1)) `div` 2 where d = dist f t

cost :: (Int -> Int -> Int) -> Int -> [Int] -> Int
cost d n = sum . map (d n)

part1 :: [Int] -> Int
part1 inp = cost dist i inp
    where i = median inp

part2 :: [Int] -> Int
part2 inp =  minimum [cost dist' i inp | i <- [min..max]]
    where p = median inp
          min = minimum inp
          max = maximum inp

day07 :: String -> (String, String)
day07 str = (show $ part1 inp, show $ part2 inp) where inp = parse str
