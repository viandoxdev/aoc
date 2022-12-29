module Challenges.Day04 where

import Utils
import Data.List
import Data.Array (Array, accumArray, (!))
import Data.Functor
import Data.Maybe (mapMaybe)

parse :: String -> ([Int], [[[Int]]])
parse s = ( map read $ splitOn "," $ head blocks
          , map (map (map read . words) . lines) $ tail blocks)
    where blocks = splitOn "\n\n" s

drawSet :: [Int] -> (Int, Array Int Bool)
drawSet list = (head list, accumArray (||) False (0,99) [(i, True) | i <- list])

boardScore :: Int -> Array Int Bool -> [[Int]] -> Int
boardScore m s = (*m) . sum  . filter (not . (s!)) . concat

won ::  Array Int Bool -> [[Int]] -> Bool
won s b = any (all (s!)) b || any (all (s!)) (transpose b)

run :: [[[Int]]] -> [Int] -> [Int] -> [Int]
run [] _ _ = []
run boards draw todraw = map (boardScore m s) w ++ run l (head todraw : draw) (tail todraw) where
    (m, s) = drawSet draw
    (w, l) = partition (won s) boards

solve :: ([Int] -> Int) -> ([Int], [[[Int]]]) -> Int
solve f (draws, boards) = f $ run boards [head draws] (tail draws)

day04 :: String -> (String, String)
day04 str = (show $ solve head inp, show $ solve last inp) where inp = parse str
