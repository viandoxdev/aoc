module Main where

import Aoc
import Challenges.Day1
import Challenges.Day2
import Challenges.Day3
import Challenges.Day4
import Challenges.Day5
import Challenges.Day6

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
    [ day1
    , day2 
    , day3 
    , day4 
    , day5 
    , day6 
    ]

main :: IO ()
main = do
    aoc <- newAoc
    putStrLn "Aoc 2021"
    putStrLn ""

    runDays aoc $ zip [1..] days

    putStrLn ""
    putStrLn "Done"

