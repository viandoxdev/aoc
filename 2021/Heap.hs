{-# LANGUAGE NamedFieldPuns #-}

module Heap (heapNew, heapPush, heapPop, STHeap, heapSize, heapList, heapAny, heapFind, heapUpdate, heapUpdateAt) where

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Array.ST (STArray, newArray_, readArray, writeArray)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)

data STHeap s v = STHeapCons
    { nodes :: STArray s Int (Int, v)
    , size :: STRef s Int
    , cap :: Int
    }

heapNew :: Int -> ST s (STHeap s v)
heapNew cap = do
    nodes <- newArray_ (0, cap - 1)
    size <- newSTRef 0
    return STHeapCons{nodes, size, cap}

heapSize :: STHeap s v -> ST s Int
heapSize = readSTRef . size

heapPush :: STHeap s v -> Int -> v -> ST s ()
heapPush h p v = do
    sz <- readSTRef (size h)

    writeSTRef (size h) (sz + 1)
    writeArray (nodes h) sz (p, v)
    bubbleUp h sz

heapAny :: STHeap s v -> ((Int, v) -> Bool) -> ST s Bool
heapAny h e = do
    sz <- heapSize h
    or <$> mapM (fmap e . readArray (nodes h)) [0 .. sz - 1]

heapFind :: STHeap s v -> ((Int, v) -> Bool) -> ST s (Maybe Int)
heapFind h e = do
    sz <- heapSize h
    elemIndex True <$> mapM (fmap e . readArray (nodes h)) [0 .. sz - 1]

heapUpdate :: STHeap s v -> ((Int, v) -> Bool) -> Int -> v -> ST s ()
heapUpdate h e p v = do
    i <- heapFind h e
    heapUpdateAt h (fromJust i) p v

heapUpdateAt :: STHeap s v -> Int -> Int -> v -> ST s ()
heapUpdateAt h i p v = do
    sz <- heapSize h

    if i < 0 || i >= sz
        then error "Can't update invalid index"
        else do
            o <- fst <$> readArray (nodes h) i
            writeArray (nodes h) i (p, v)

            if o <= p
                then heapify h i
                else do
                    bubbleUp h i

heapList :: STHeap s v -> ST s [Int]
heapList h = do
    sz <- heapSize h
    mapM (fmap fst . readArray (nodes h)) [0 .. sz - 1]

heapPop :: STHeap s v -> ST s v
heapPop h = do
    let n = nodes h
    sz <- subtract 1 <$> heapSize h

    if sz < 0
        then error "Can't pop from empty queue"
        else do
            root <- readArray n 0
            leaf <- readArray n sz
            writeArray n 0 leaf
            writeSTRef (size h) sz
            heapify h 0
            return $ snd root

safeGet :: STArray s Int (Int, v) -> Int -> Int -> ST s Int
safeGet n sz i = if i < sz then fst <$> readArray n i else return maxBound

heapify :: STHeap s v -> Int -> ST s ()
heapify h i = do
    let n = nodes h
    let (l, r) = (2 * i + 1, 2 * i + 2)

    sz <- heapSize h

    lc <- safeGet n sz l
    rc <- safeGet n sz r
    mc <- fst <$> readArray n i

    let c
            | lc < mc && lc <= rc = l
            | rc < mc && rc <= lc = r
            | otherwise = i

    when (c /= i) $ do
        a <- readArray n i
        b <- readArray n c
        writeArray n i b
        writeArray n c a
        heapify h c

bubbleUp :: STHeap s v -> Int -> ST s ()
bubbleUp h i = do
    let j = (i - 1) `div` 2
    let n = nodes h
    when (j >= 0) $ do
        (c, cv) <- readArray n i
        (p, pv) <- readArray n j
        when (c < p) $ do
            writeArray n i (p, pv)
            writeArray n j (c, cv)
            bubbleUp h j
