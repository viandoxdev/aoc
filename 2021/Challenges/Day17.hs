module Challenges.Day17 (day17) where

import Utils (splitOn)
import Data.Ix (range, inRange)

type Vec2 = (Int, Int)

parse :: String -> (Vec2, Vec2)
parse s = ((a, c), (b, d))
    where [_, a, b, _, c, d] = map read $ concatMap (splitOn ", ") $ concatMap (splitOn "..") $ splitOn "=" s

hits' :: (Vec2, Vec2) -> Vec2 -> Vec2 -> Bool
hits' bnd@((_,my),(mx,_)) (x,y) (vx,vy) | inRange bnd (x,y) = True
                  | x > mx || y < my = False
                  | otherwise = hits' bnd (x + vx, y + vy) (max 0 $ vx - 1, vy - 1)

hits :: (Vec2, Vec2) -> Vec2 -> Bool
hits bnd = hits' bnd (0,0)

-- initially solved on desmos: https://www.desmos.com/calculator/4s1zvn1xbe
part1 :: (Vec2, Vec2) -> Int
part1 (_, (y0, _)) = (y * (y + 1)) `div` 2 where y = abs y0 - 1

part2 :: (Vec2, Vec2) -> Int
part2 b@((x0,y0), (x1,_)) = length $ filter (hits b) $ range ((minx, miny), (maxx, maxy))
    where minx = ceiling (sqrt (2 * (fromIntegral :: Int -> Double) x0 + 0.25) - 0.5)
          maxx = x1
          miny = y0
          maxy = abs y0 - 1

day17 :: String -> (String, String)
day17 str = (show $ part1 inp, show $ part2 inp) where inp = parse str
