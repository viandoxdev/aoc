module Challenges.Day5 where
import Utils (splitOn)
import Data.Array (Array, array, elems, accumArray, assocs)
import Data.Bits (Bits(shiftL))
import Data.Maybe (catMaybes)
import Control.Monad ((<=<))
import Data.List (group, sort)

type Vec2 = (Int, Int)

line :: (Vec2, Vec2) -> [Vec2]
line ((x0, y0), (x1, y1)) = [(x0 + s * sx, y0 + s * sy) | s <- [0..l]] where
    (dx, dy) = (x1 - x0, y1 - y0)
    (sx, sy) = (signum dx, signum dy)
    l = max (abs dx) (abs dy)

parse :: String -> [(Vec2, Vec2)]
parse = map (parseLine . words) . lines where
    parseVec2 [x,y] = (read x, read y)
    parseVec2 _ = error "Bad input"
    parseLine [l, _, r] = (parseVec2 $ splitOn "," l, parseVec2 $ splitOn "," r)
    parseLine _ = error "Bad input"

flatten :: Vec2 -> Int
flatten (x, y) = y * 1000 + x

axisAligned :: (Vec2, Vec2) -> Bool
axisAligned ((a, b), (c, d)) = a == c || b == d

overlapping :: [Int] -> Bool
overlapping [_] = False
overlapping _ = True

solve :: [(Vec2, Vec2)] -> Int
solve x = final where
    base = map flatten $ x >>= line
    sorted = sort base
    grouped = group sorted
    final = length $ filter overlapping grouped

part1 :: [(Vec2, Vec2)] -> Int
part1 = solve . filter axisAligned

part2 :: [(Vec2, Vec2)] -> Int
part2 = solve

day5 :: String -> (String, String)
day5 str = (show $ part1 inp, show $ part2 inp) where inp = parse str
