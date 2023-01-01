module Challenges.Day06 (day06) where
import Utils (splitOn)
import Data.Array (accumArray, assocs)

flatten :: [(Int, Int)] -> [(Int, Int)]
flatten = assocs . accumArray (+) 0 (0, 8)

parse :: String -> [(Int, Int)]
parse =  flatten . map m . splitOn "," where
    m s = (read s, 1)

tick :: [(Int, Int)] -> [(Int, Int)]
tick = flatten . (>>= tickf) where
    tickf (0, n) = [(8, n), (6, n)]
    tickf (d, n) = [(d - 1, n)]

tick7 :: [(Int, Int)] -> [(Int, Int)]
tick7 = flatten . (>>= tickf) where
    tickf (8, n) = [(1, n)]
    tickf (7, n) = [(0, n)]
    tickf (d, n) = [(d, n), (d + 2, n)]

count :: [(Int, Int)] -> Int
count = sum . map snd

run :: Int -> [(Int, Int)] -> [(Int, Int)]
run n = (!!t1) . iterate tick . (!!t7) . iterate tick7
    where (t7, t1) =  n `divMod` 7

part1 :: [(Int, Int)] -> Int
part1 = count . run 80

part2 :: [(Int, Int)] -> Int
part2 = count . run 256

day06 :: String -> (String, String)
day06 str = (show $ part1 inp, show $ part2 inp) where inp = parse str
