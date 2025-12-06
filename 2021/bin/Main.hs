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
import Challenges.Day16
import Challenges.Day17
import Challenges.Day18
import Challenges.Day19

runDay :: Aoc -> Int -> (String -> (String, String)) -> IO (Int, Double)
runDay aoc day f = do
    putStrLn $ "[Day " ++ show day ++ "]"
    input <- getInput aoc 2021 day

    start <- getTime Monotonic
    let !(!part1, !part2) = f input
    end <- getTime Monotonic

    putStrLn $ "  Part1: " ++ part1
    putStrLn $ "  Part2: " ++ part2
    return (day, msTs $ diffTimeSpec end start)

runDays :: Aoc -> [(Int, String -> (String, String))] -> IO [(Int, Double)]
runDays _ [] = return []
runDays aoc ds = mapM (uncurry (runDay aoc)) ds

msTs :: TimeSpec -> Double
msTs = (/100) . (fromIntegral . (`div` 10000) . toNanoSecs :: TimeSpec -> Double)

wholeDigits :: Double -> Int
wholeDigits x | x < 1.0 = 1
              | otherwise = (+1) $ floor $ logBase 10 $ (realToFrac :: (Double -> Float)) x

showI :: Int -> Int -> String
showI digits i = leftPad ++ str
    where str = show i
          leftPad = replicate (digits - length str) ' '

showF :: Int -> Int -> Double -> String
showF digits places d = leftPad ++ take len str ++ rightPad
    where str = show d 
          dts = wholeDigits d
          len = dts + 1 + places
          leftPad = replicate (digits - dts) ' '
          rightPad = replicate (len - length str) '0'

printTime :: (Int, Double) -> IO ()
printTime (day, ts) = putStrLn $ "  Day " ++ showI 2 day ++ ": " ++ col ts where
    col v | v < 015.0 = "\^[[92m" ++ s v ++ "ms\^[[0m"
          | v < 100.0 = "\^[[93m" ++ s v ++ "ms\^[[0m"
          | otherwise = "\^[[91m" ++ s v ++ "ms\^[[0m"
    s = showF 3 2

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
    , (16, day16)
    , (17, day17)
    , (18, day18)
    , (19, day19)
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
    putStrLn $ "Sum: " ++ showF 4 3 total ++ "ms (sum of individual runtime)"
    putStrLn $ "Total: " ++ showF 4 3 (msTs totalMeasured) ++ "ms (total program runtime including IO)"

    putStrLn ""
    putStrLn "Done"
