{-# LANGUAGE TupleSections #-}

module Challenges.Day19 (day19) where

import Control.Monad ((<=<))
import Data.List (delete, intersect, tails, transpose)
import Data.Maybe (listToMaybe, mapMaybe)
import Debug.Trace (traceShow)
import Utils (splitOn)

type Vec3 = (Int, Int, Int)

v3Sub :: Vec3 -> Vec3 -> Vec3
v3Sub (a, b, c) (d, e, f) = (a - d, b - e, c - f)

v3rot :: Vec3 -> [Vec3]
v3rot (x, y, z) =
    [ (-x, -y, z)
    , (-x, -z, -y)
    , (-x, y, -z)
    , (-x, z, y)
    , (-y, -x, -z)
    , (-y, -z, x)
    , (-y, x, z)
    , (-y, z, -x)
    , (-z, -x, y)
    , (-z, -y, -x)
    , (-z, x, -y)
    , (-z, y, x)
    , (x, -y, -z)
    , (x, -z, y)
    , (x, y, z)
    , (x, z, -y)
    , (y, -x, z)
    , (y, -z, -x)
    , (y, x, -z)
    , (y, z, x)
    , (z, -x, -y)
    , (z, -y, x)
    , (z, x, y)
    , (z, y, -x)
    ]

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

v3sdist :: Vec3 -> Vec3 -> Int
v3sdist a b = x * x + y * y + z * z
  where
    (x, y, z) = a `v3Sub` b

elems :: [a] -> [(a, [a])]
elems [] = []
elems [x] = [(x, [])]
elems l = fst $ foldl f ([], l) l
  where
    f (xs, l) e = ((e, tail l) : xs, tail l ++ [head l])

parse :: String -> [[Vec3]]
parse = map parseScanner . splitOn "\n\n"
  where
    parseScanner = map (parseVec3 . map (read :: [Char] -> Int) . splitOn ",") . tail . lines
    parseVec3 [x, y, z] = (x, y, z)
    parseVec3 _ = error "Bad Input"

scannerDists :: [Vec3] -> [[Int]]
scannerDists xs = [[v3sdist a b | a <- xs] | b <- xs]

share12 :: Ord a => [a] -> [a] -> Bool
share12 xs = (>= 12) . length . intersect xs

mergeScanners :: [Vec3] -> [Vec3] -> Maybe [Vec3]
mergeScanners s1 s2 = Nothing
  where
    s1d = zip s1 (scannerDists s1)
    s2d = zip s2 (scannerDists s2)

processOptions :: Vec3 -> [(Vec3, Vec3, [Vec3])] -> [(Vec3, Vec3, [Vec3])]
processOptions diff = (>>= f)
  where
    check last new = diff == (new `v3Sub` last)
    f (last, _, opts) = map (\new -> (new, new `v3Sub` last, delete new opts)) $ filter (check last) opts

advance :: (Vec3, Vec3, [Vec3]) -> [(Vec3, Vec3, [Vec3])] -> Maybe [Vec3]
advance (last, diff, opts) pos = listToMaybe $ mapMaybe (next . update) opts
  where
    next (_, []) = Nothing
    next (_, [(_, _, x)]) = Just x
    next (s, p) = advance s p
    update new = ((new, new `v3Sub` last, delete new opts), processOptions (new `v3Sub` last) pos)

mergeScannersRot :: [Vec3] -> [Vec3] -> Maybe [Vec3]
mergeScannersRot s1 = listToMaybe . mapMaybe (mergeScanners s1) . transpose . map v3rot

edges :: [a] -> [(a, a)]
edges = f <=< tails
  where
    f (x : xs) = map (x,) xs

directedEdges :: [a] -> [(a, a)]
directedEdges = (\(a, b) -> [(a, b), (b, a)]) <=< edges

overlap :: [Vec3] -> [Vec3] -> Bool
overlap a b = 
  where
    ea = directedEdges a
    eb = directedEdges b

solve :: [[Vec3]] -> Int
solve [s] = length s
solve (s : xs) = traceShow (mapMaybe f $ elems (xs :: [[Vec3]])) 0
  where
    f (s2, o) = (: o) <$> mergeScannersRot s s2

day19 :: String -> (String, String)
day19 str = (show $ solve inp, show $ solve inp) where inp = parse str
