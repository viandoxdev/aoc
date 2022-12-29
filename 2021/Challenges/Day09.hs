module Challenges.Day09 where
import Data.Char (digitToInt)
import Utils (windows, transpose', chunks)
import Debug.Trace
import Data.List
import Data.Array
import Data.Array.ST (STArray, newArray, writeArray, readArray, runSTArray)
import Control.Monad.ST
import Data.Maybe (fromMaybe)
import GHC.Arr (unsafeFreezeSTArray)

parse :: String -> [[Int]]
parse = map (map digitToInt) . lines

grid :: [[Int]] -> Array (Int, Int) Int
grid a = listArray ((0, 0), (width, height)) $ concat $ transpose a
    where height = length a - 1
          width = length (head a) - 1

(!?) :: Ix i => Array i e -> i -> Maybe e
(!?) a i = if inRange (bounds a) i then Just (a!i) else Nothing

lowPoints :: Array (Int, Int) Int -> [(Int, Int)]
lowPoints grid = filter lp $ indices grid where
    (>>) i h = maybe True (>h) (grid!?i)
    lp (x, y) = all (>>h) [(x,y-1),(x-1,y),(x,y+1),(x+1,y)] where h = grid!(x,y)

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
    array <- newArray (bounds grid) False
    floodST array grid point

day09 :: String -> (String, String)
day09 str = ( show $ sum $ map (pointScore g) lps
           , show $ product $ take 3 $ sortBy (flip compare) $ map (flood g) lps )
    where g = grid $ parse str
          lps = lowPoints g

