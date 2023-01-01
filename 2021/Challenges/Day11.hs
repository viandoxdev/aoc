{-# LANGUAGE TupleSections #-}
module Challenges.Day11 where
import Data.Array
import Data.Char (digitToInt)
import Data.List (transpose, intercalate, group, sort, mapAccumL)

dim = 10

parse :: String -> Array (Int, Int) Int
parse = listArray ((0,0), (dim-1,dim-1)) . concat . transpose . map (map digitToInt) . lines

inc :: Array (Int, Int) Int -> [(Int, Int)] -> Array (Int, Int) Int
inc a [] = a
inc a xs = inc nextGrid nextInc
    where neighbours (x, y) = [(x-1,y-1),(x,y-1),(x+1,y-1),(x-1,y),(x+1,y),(x-1,y+1),(x,y+1),(x+1,y+1)]
          incremented = accum (+) a (map (,1) xs)
          flash = filter ((\x -> x > 9 && x < 100) . (incremented!)) (map head $ group $ sort xs)
          nextGrid = incremented // map (,100) flash
          nextInc = filter (inRange (bounds a)) $ (>>= neighbours) flash

flash :: Array (Int, Int) Int -> (Array (Int, Int) Int, Int)
flash a = (a // updt, length updt)
    where updt = map (\(i, _) -> (i, 0)) $ filter ((>9) . snd) $ assocs a

step :: Array (Int, Int) Int -> (Array (Int, Int) Int, Int)
step = flash . (inc <*> indices)

step' :: (Array (Int, Int) Int, Int) -> (Array (Int, Int) Int, Int)
step' (a, _) = step a

part1 :: Array (Int, Int) Int -> Int
part1 = sum . map snd . take 101 . iterate step' . (,0) -- off by one, not sure why

part2 :: Array (Int, Int) Int -> Int
part2 = length . takeWhile (<100) . map snd . iterate step' . (, 0)

day11 :: String -> (String, String)
day11 str = (show $ part1 inp, show $ part2 inp) where inp = parse str
