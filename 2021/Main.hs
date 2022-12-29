module Main where

import Aoc
import Challenges.Day1
import Challenges.Day2
import Challenges.Day3
import Challenges.Day4
import Challenges.Day5
import Challenges.Day6
import Challenges.Day7
import Challenges.Day8
import Challenges.Day9
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
    [ ( 1,  day1)
    , ( 2,  day2) 
    , ( 3,  day3) 
    , ( 4,  day4) 
    , ( 5,  day5) 
    , ( 6,  day6) 
    , ( 7,  day7) 
    , ( 8,  day8) 
    , ( 9,  day9) 
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

