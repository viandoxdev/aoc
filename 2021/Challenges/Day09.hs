module Challenges.Day09 (day09) where
import Data.Char (digitToInt)
import Data.List
import Data.Array
import Data.Array.ST (STArray, newArray, writeArray, readArray)
import Control.Monad.ST

parse :: String -> [[Int]]
parse = map (map digitToInt) . lines

makeGrid :: [[Int]] -> Array (Int, Int) Int
makeGrid a = listArray ((0, 0), (width, height)) $ concat $ transpose a
    where height = length a - 1
          width = length (head a) - 1

(!?) :: Ix i => Array i e -> i -> Maybe e
(!?) a i = if inRange (bounds a) i then Just (a!i) else Nothing

lowPoints :: Array (Int, Int) Int -> [(Int, Int)]
lowPoints grid = filter lp $ indices grid where
    lp (x, y) = all (maybe True (>h) . (grid!?)) [(x,y-1),(x-1,y),(x,y+1),(x+1,y)] 
        where h = grid!(x,y)

pointScore :: Array (Int, Int) Int -> (Int, Int) -> Int
pointScore grid = (+1) . (grid!)

floodST :: STArray s (Int, Int) Bool -> Array (Int, Int) Int -> (Int, Int) -> ST s Int
floodST visited grid (x, y) = if maybe True (>=9) $ grid!?(x,y) then return 0 else do
    isvisited <- readArray visited (x,y)
    if isvisited then return 0 else do
        writeArray visited (x,y) True
        let next = [(x,y-1),(x-1,y),(x,y+1),(x+1,y)]
        (1+) . sum <$> mapM (floodST visited grid) next

flood :: Array (Int, Int) Int -> (Int, Int) -> Int
flood grid point = runST $ do
    arr <- newArray (bounds grid) False
    floodST arr grid point

day09 :: String -> (String, String)
day09 str = ( show $ sum $ map (pointScore grid) lps
           , show $ product $ take 3 $ sortBy (flip compare) $ map (flood grid) lps )
    where grid = makeGrid $ parse str
          lps = lowPoints grid

