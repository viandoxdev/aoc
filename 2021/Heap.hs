{-# LANGUAGE NamedFieldPuns #-}
module Heap (heapNew, heapPush, heapPop, STHeap, heapSize, heapFind, heapAny, heapUpdateAt, heapUpdate, heapList) where
import Data.Array.ST (STArray, newArray_, writeArray, readArray)
import Control.Monad.ST ( ST )
import Data.STRef (STRef, newSTRef, writeSTRef, readSTRef)
import Data.Array (bounds)
import Control.Monad (when, unless, forM_)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

data STHeap s v = STHeapCons
    { nodes :: STArray s Int v
    , size :: STRef s Int
    , cap :: Int
    , cmp :: v -> v -> Bool }

heapNew :: Int -> (v -> v -> Bool) -> Control.Monad.ST.ST s (STHeap s v)
heapNew cap cmp = do
    nodes <- newArray_ (0, cap - 1)
    size <- newSTRef 0
    return STHeapCons {nodes, size, cmp, cap}

heapSize :: STHeap s v -> ST s Int
heapSize = readSTRef . size

heapPush :: STHeap s v -> v -> ST s ()
heapPush h v = do
    sz <- readSTRef (size h)

    if sz > cap h then
        error "Heap capacity overflow"
    else do
        writeSTRef (size h) (sz + 1)
        writeArray (nodes h) sz v
        bubbleUp h sz

heapAny :: STHeap s v -> (v -> Bool) -> v -> ST s Bool
heapAny h e v = do
    sz <- heapSize h
    or <$> mapM (fmap e . readArray (nodes h)) [0..sz - 1]

heapFind :: STHeap s v -> (v -> Bool) -> ST s (Maybe Int)
heapFind h e = do
    sz <- heapSize h
    elemIndex True <$> mapM (fmap e . readArray (nodes h)) [0..sz - 1]

heapUpdate :: Show v => STHeap s v -> (v -> Bool) -> v -> ST s ()
heapUpdate h e v = do
    i <- heapFind h e
    heapUpdateAt h (fromJust i) v

heapUpdateAt :: Show v => STHeap s v -> Int -> v -> ST s ()
heapUpdateAt h i v = do
    sz <- heapSize h

    if i < 0 || i >= sz then
        error "Can't update invalid index"
    else do
        p <- readArray (nodes h) i
        writeArray (nodes h) i v

        if cmp h p v then
            heapify h i
        else do
            bubbleUp h i

heapList :: STHeap s v -> ST s [v] 
heapList h = do
    sz <- heapSize h
    mapM (readArray (nodes h)) [0..sz - 1]

heapPop :: STHeap s v -> ST s v
heapPop h = do
    let n = nodes h
    sz <- subtract 1 <$> readSTRef (size h)

    if sz < 0 then
        error "Can't pop from empty queue"
    else do
        root <- readArray n 0
        leaf <- readArray n sz
        writeArray n 0 leaf
        writeSTRef (size h) sz
        heapify h 0
        return root

heapify :: STHeap s v -> Int -> ST s ()
heapify h i = do
    let f = cmp h
    let n = nodes h
    let (l, r) = (2 * i + 1, 2 * i + 2)

    sz <- readSTRef $ size h
    m <- newSTRef i

    when (l < sz) $ do
        c <- readSTRef m
        lc <- readArray n l
        mc <- readArray n c
        when (f lc mc) $ writeSTRef m l
    when (r < sz) $ do
        c <- readSTRef m
        rc <- readArray n r
        mc <- readArray n c
        when (f rc mc) $ writeSTRef m r
    c <- readSTRef m
    when (c /= i) $ do
        a <- readArray n i
        b <- readArray n c
        writeArray n i b
        writeArray n c a
        heapify h c

bubbleUp :: STHeap s v -> Int -> ST s ()
bubbleUp h i = do
    let pi = (i - 1) `div` 2
    let n = nodes h
    when (pi >= 0) $ do
        c <- readArray n i
        p <- readArray n pi
        when (cmp h c p) $ do
            writeArray n i p
            writeArray n pi c
            bubbleUp h pi
