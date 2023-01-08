{-# LANGUAGE BangPatterns #-}
module Main where

import Aoc
import System.Clock

import Challenges.Day01
import Challenges.Day02
import Challenges.Day03
import Challenges.Day04
import Challenges.Day05
import Challenges.Day06
import Challenges.Day07
import Challenges.Day08
import Challenges.Day09
import Challenges.Day10
import Challenges.Day11
import Challenges.Day12
import Challenges.Day13
import Challenges.Day14
import Challenges.Day15
import Heap
import Control.Monad.ST
import Debug.Trace (traceShow)

runDay :: Aoc -> Int -> (String -> (String, String)) -> IO (Int, TimeSpec)
runDay aoc day f = do
    putStrLn $ "[Day " ++ show day ++ "]"
    input <- getInput aoc 2021 day

    start <- getTime Monotonic
    let !(!part1, !part2) = f input
    end <- getTime Monotonic

    putStrLn $ "  Part1: " ++ part1
    putStrLn $ "  Part2: " ++ part2
    return (day, diffTimeSpec end start)

runDays :: Aoc -> [(Int, String -> (String, String))] -> IO [(Int, TimeSpec)]
runDays _ [] = return []
runDays aoc ds = mapM (uncurry (runDay aoc)) ds

showTs :: TimeSpec -> String
showTs = (++ "ms") . show . (/100) . (fromIntegral . (`div` 10000) . toNanoSecs :: TimeSpec -> Double)

printTime :: (Int, TimeSpec) -> IO ()
printTime (day, ts) = putStrLn $ "  Day " ++ show day ++ ": " ++ showTs ts

dayDummy :: String -> (String, String)
dayDummy _ = ("none", "none")

days :: [(Int, String -> (String, String))]
days =
    [ (01, day01)
    , (02, day02)
    , (03, day03)
    , (04, day04)
    , (05, day05)
    , (06, day06)
    , (07, day07)
    , (08, day08)
    , (09, day09)
    , (10, day10)
    , (11, day11)
    , (12, day12)
    , (13, day13)
    , (14, day14)
    , (15, day15)
    ]

main :: IO ()
main = do
    aoc <- newAoc
    putStrLn "Aoc 2021"
    putStrLn ""

    start <- getTime Monotonic
    results <- runDays aoc days
    end <- getTime Monotonic

    putStrLn ""
    putStrLn "Timings"

    mapM_ printTime results

    let total = foldl (\a e -> a + snd e) 0 results
    let totalMeasured = diffTimeSpec end start

    putStrLn ""
    putStrLn $ "Sum: " ++ showTs total
    putStrLn $ "Total: " ++ showTs totalMeasured

    putStrLn ""
    putStrLn "Done"
