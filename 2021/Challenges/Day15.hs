{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Challenges.Day15 (day15) where
import Data.Char (digitToInt)
import Data.Array (Array, listArray, bounds, Ix (..), (!), indices, assocs, elems, array)
import Data.List (transpose, sort, sortBy)
import Data.Array.ST (STArray, newArray, writeArray, readArray, freeze)
import Control.Monad.ST (ST, runST)
import Data.Ord (comparing)
import Control.Monad (forM_, when, unless)
import Heap ( STHeap, heapNew, heapPush, heapPop, heapSize, heapUpdate, heapList )
import Data.STRef (readSTRef, writeSTRef, newSTRef)
import Debug.Trace (traceShowId, trace, traceShow)
import Utils (chunks)

type Vec2 = (Int, Int)

parse :: String -> Array Vec2 Int
parse str = listArray ((0,0), (w,h)) $ concat $ transpose $ map (map digitToInt) ls
    where ls = lines str
          h = length ls - 1
          w = length (head ls) - 1

tile :: Vec2 -> Vec2 -> (Vec2, Int, Int) -> String
tile u t (p,d,h) | p == u = strcol (235,168,62) ++ s
                 | p == t = strcol (52,97,235) ++ s
                 | otherwise = strcol (interp (255,255,255) (255,0,0) d 200) ++ s
    where s = ' ' : show h


djikstra' :: Array Vec2 Int -> STArray s Vec2 Int -> STArray s Vec2 Bool -> Vec2 -> STHeap s (Int, Vec2) -> ST s Int
djikstra' graph dist open to queue = do
    (_, u) <- heapPop queue
    writeArray open u False

    if u == to then
        readArray dist u
    else do
        let (x,y) = u
        let neighbours = filter (inRange (bounds graph)) [(x,y-1),(x-1,y),(x,y+1),(x+1,y)]

        du <- readArray dist u
        forM_ neighbours $ \v -> do
            dv <- readArray dist v
            let alt = du + graph!v

            when (alt < dv) $ do
                writeArray dist v alt
                o <- readArray open v
                unless o $ do
                    heapPush queue (alt, v)
                    writeArray open v True

        djikstra' graph dist open to queue

strcol :: (Int, Int, Int) -> String
strcol (r, g, b) = "\^[[48;2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m"

interp :: (Int, Int, Int) -> (Int, Int, Int) -> Int -> Int -> (Int, Int, Int)
interp (r,g,b) (s,h,c) v m = (i r s t, i g h t, i b c t) where
    t = min (fromIntegral v / fromIntegral m) 1 :: Double
    i a d w = (floor $ fromIntegral ((a :: Int) + ((d :: Int) - a)) * (w :: Double)) :: Int

djikstra :: Array Vec2 Int -> Vec2 -> Vec2 -> Int
djikstra graph from to = runST $ do
    dist <- newArray (bounds graph) 100000
    open <- newArray (bounds graph) False
    queue <- heapNew 1024 (\(a,_) (b,_) -> a <= b)
    writeArray dist from 0
    heapPush queue (0, from)
    djikstra' graph dist open to queue

wrap :: Int -> Int
wrap = (+1) . (`mod` 9) . subtract 1

upscale :: Array Vec2 Int -> Array Vec2 Int
upscale g = array ((0,0), (-1,-1) `add` (bnd `mul` (5,5))) $ concatMap f $ assocs g where
    bnd = add (1,1) $ snd $ bounds g
    f (i, e) = map (\m -> (i `add` (m `mul` bnd), wrap $ e + uncurry (+) m)) $ range ((0,0),(4,4))
    add (a,b) (c,d) = (a+c, b+d)
    mul (a,b) (c,d) = (a*c, b*d)

solve :: Array Vec2 Int -> Int
solve g = (uncurry $ djikstra g) (bounds g)

day15 :: String -> (String, String)
day15 str = (show $ solve inp, show $ solve $ upscale inp) where inp = parse str
