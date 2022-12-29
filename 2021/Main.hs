module Main where

import Aoc
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

runDay :: (Show n, Integral n) => Aoc -> n -> (String -> (String, String)) -> IO ()
runDay aoc day f = do
    putStrLn $ "[Day " ++ show day ++ "]"
    input <- getInput aoc 2021 day
    let (part1, part2) = f input
    putStrLn $ "  Part1: " ++ part1
    putStrLn $ "  Part2: " ++ part2

runDays :: (Show n, Integral n) => Aoc -> [(n, String -> (String, String))] -> IO ()
runDays aoc [] = return ()
runDays aoc days = do
    let (day, f) = head days
    runDay aoc day f
    runDays aoc (tail days)

dayDummy :: String -> (String, String)
dayDummy _ = ("none", "none")

days = 
    [ ( 1, day01)
    , ( 2, day02) 
    , ( 3, day03) 
    , ( 4, day04) 
    , ( 5, day05) 
    , ( 6, day06) 
    , ( 7, day07) 
    , ( 8, day08) 
    , ( 9, day09) 
    , (10, day10) 
    ]

main :: IO ()
main = do
    aoc <- newAoc
    putStrLn "Aoc 2021"
    putStrLn ""

    runDays aoc days

    putStrLn ""
    putStrLn "Done"

