{-# LANGUAGE ScopedTypeVariables #-}

module Challenges.Day15 (day15, day15') where

import Control.Monad (forM_, unless, when)
import Control.Monad.ST (ST, runST)
import Data.Array (Array, Ix (..), array, assocs, bounds, listArray, (!))
import Data.Array.ST (STArray, newArray, readArray, writeArray)
import Data.Char (digitToInt)
import Data.List (transpose)
import Heap (STHeap, heapNew, heapPop, heapPush)

type Vec2 = (Int, Int)

parse :: String -> Array Vec2 Int
parse str = listArray ((0, 0), (w, h)) $ concat $ transpose $ map (map digitToInt) ls
  where
    ls = lines str
    h = length ls - 1
    w = length (head ls) - 1

heuristics :: Vec2 -> Vec2 -> Int
heuristics (gx, gy) (x, y) = abs (x - gx) + abs (y - gy)

aStar' :: Array Vec2 Int -> STArray s Vec2 Int -> STArray s Vec2 Int -> STArray s Vec2 Bool -> Vec2 -> STHeap s Vec2 -> ST s Int
aStar' graph dist score open to queue = do
    u <- heapPop queue
    writeArray open u False

    if u == to
        then readArray dist u
        else do
            let (x, y) = u
            let neighbours = filter (inRange (bounds graph)) [(x, y - 1), (x - 1, y), (x, y + 1), (x + 1, y)]

            du <- readArray dist u
            forM_ neighbours $ \v -> do
                dv <- readArray dist v
                let alt = du + graph ! v
                let s = alt + heuristics to v

                when (alt < dv) $ do
                    writeArray dist v alt
                    writeArray score v s
                    o <- readArray open v
                    unless o $ do
                        heapPush queue s v
                        writeArray open v True

            aStar' graph dist score open to queue

djikstra' :: Array Vec2 Int -> STArray s Vec2 Int -> STArray s Vec2 Bool -> Vec2 -> STHeap s Vec2 -> ST s Int
djikstra' graph dist open to queue = do
    u <- heapPop queue
    writeArray open u False

    if u == to
        then readArray dist u
        else do
            let (x, y) = u
            let neighbours = filter (inRange (bounds graph)) [(x, y - 1), (x - 1, y), (x, y + 1), (x + 1, y)]

            du <- readArray dist u
            forM_ neighbours $ \v -> do
                dv <- readArray dist v
                let alt = du + graph ! v

                when (alt < dv) $ do
                    writeArray dist v alt
                    o <- readArray open v
                    unless o $ do
                        heapPush queue alt v
                        writeArray open v True

            djikstra' graph dist open to queue

aStar :: Array Vec2 Int -> Vec2 -> Vec2 -> Int
aStar graph from to = runST $ do
    dist <- newArray (bounds graph) maxBound
    score <- newArray (bounds graph) maxBound
    open <- newArray (bounds graph) False
    queue <- heapNew 2048
    writeArray dist from 0
    let s = heuristics to from
    writeArray score from s
    heapPush queue s from
    aStar' graph dist score open to queue

djikstra :: Array Vec2 Int -> Vec2 -> Vec2 -> Int
djikstra graph from to = runST $ do
    dist <- newArray (bounds graph) maxBound
    open <- newArray (bounds graph) False
    queue <- heapNew 2048
    writeArray dist from 0
    heapPush queue 0 from
    djikstra' graph dist open to queue

wrap :: Int -> Int
wrap = (+ 1) . (`mod` 9) . subtract 1

upscale :: Array Vec2 Int -> Array Vec2 Int
upscale g = array ((0, 0), (-1, -1) `add` (bnd `mul` (5, 5))) $ concatMap f $ assocs g
  where
    bnd = add (1, 1) $ snd $ bounds g
    f (i, e) = map (\m -> (i `add` (m `mul` bnd), wrap $ e + uncurry (+) m)) $ range ((0, 0), (4, 4))
    add (a, b) (c, d) = (a + c, b + d)
    mul (a, b) (c, d) = (a * c, b * d)

solve :: Array Vec2 Int -> Int
solve g = (uncurry $ djikstra g) (bounds g)

solve' :: Array Vec2 Int -> Int
solve' g = (uncurry $ aStar g) (bounds g)

day15 :: String -> (String, String)
day15 str = (show $ solve inp, show $ solve $ upscale inp) where inp = parse str

day15' :: String -> (String, String)
day15' str = (show $ solve inp, show $ solve' $ upscale inp) where inp = parse str
