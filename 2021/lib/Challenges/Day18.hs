{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
module Challenges.Day18 (day18) where
import Data.Char (digitToInt)
import Data.List (foldl', foldl1')
import Utils (windows)

data Item = Value Int | Pair | Unpair deriving Show

parse :: String -> [[Item]]
parse = map (map f . filter (/= ',')) . lines where
    f c | c == '[' = Pair
        | c == ']' = Unpair
        | otherwise = Value $ digitToInt c

pair :: [Item] -> [Item] -> [Item]
pair a b = (Pair : (a ++ b)) ++ [Unpair]

add :: [Item] -> [Item] -> [Item]
add a b = reduce $ pair a b

split :: [Item] -> [Item]
split = rec [] where
    rec r ((Value v):xs) | v >= 10 = explode $ reverse r ++ [Pair, Value (v `div` 2), Value ((v + 1) `div` 2), Unpair] ++ xs
    rec r (x:xs) = rec (x:r) xs
    rec r [] = reverse r

addValue :: Int -> Item -> Item
addValue a (Value v) = Value $ v + a
addValue _ a = error $ "Adding to " ++ show a

data State
    = ScanLeft { depth :: Int, res :: [Item], index :: Int, left :: Maybe Int }
    | ScanSnd { res :: [Item], sec :: Int }
    | ScanRight { res :: [Item], sec :: Int }
    | ScanEnd [Item]
    deriving Show

updateAt :: (a -> a) -> Int -> [a] -> [a]
updateAt f i l = a ++ (f v : b) where (a, v:b) = splitAt i l

explode :: [Item] -> [Item]
explode = end . foldl' f (ScanLeft { depth = 0, res = [], index = 0, left = Nothing }) where
    f (ScanEnd res) i = ScanEnd $ i : res

    f (ScanRight { res, sec }) (Value v) = ScanEnd (Value (sec + v) : res)
    f (ScanRight { res, sec }) i = ScanRight { res = i : res, sec }

    f (ScanSnd { res, sec }) Unpair = ScanRight { res, sec }
    f (ScanSnd { res }) (Value v) = ScanSnd { res, sec = v }

    f (ScanLeft { depth = 5, res, left = Nothing }) (Value _) = ScanSnd { res = Value 0 : tail res, sec = 0 }
    f (ScanLeft { depth = 5, res, index = len, left = Just left }) (Value v) = ScanSnd { res = updateAt (addValue v) (len - left - 1) $ Value 0 : tail res, sec = 0 }

    f (ScanLeft { depth, res, index, left }) Pair      = ScanLeft { depth = depth + 1, res = Pair : res,    index = index + 1, left }
    f (ScanLeft { depth, res, index, left }) Unpair    = ScanLeft { depth = depth - 1, res = Unpair : res,  index = index + 1, left }
    f (ScanLeft { depth, res, index }) (Value v) = ScanLeft { depth,             res = Value v : res, index = index + 1, left = Just index }

    f (ScanSnd {}) Pair = error "unreachable"

    end (ScanEnd res) = explode $ reverse res
    end (ScanRight { res }) = explode $ reverse res
    end (ScanLeft { res }) = split $ reverse res
    end _ = error "Invalid finishing state"

reduce :: [Item] -> [Item]
reduce = explode

mag :: [Item] -> Int
mag = fst . foldl' f (0, []) . windows 2 where
    f (s, m) [Pair, _] = (s, 3 : m)
    f (s, m) [Unpair, _] = (s, 2 : tail (tail m))
    f (s, m) [Value v, Unpair] = (s + v * product m, m)
    f (s, m) [Value v, _] = (s + v * product m, 2 : tail m)
    f _ _ = error ""

pairs :: [a] -> [(a, a)]
pairs list = concatMap (\(x:xs) -> map (x,) xs) m
    where l = length list
          m = map (take l) $ take l $ iterate (drop 1) (cycle list)

part1 :: [[Item]] -> Int
part1 = mag . foldl1' add

-- we take advantage of the fact* that mag x >= mag (reduce x)
-- to avoid reducing unnecessarily (at the cost of running mag twice as often)
-- * not proven, but verrified for all additions on my input
part2 :: [[Item]] -> Int
part2 = foldl' f 0 . pairs where
    f m (a, b) | mag p <= m = m
               | otherwise = max m (mag $ reduce p)
        where p = pair a b

day18 :: String -> (String, String)
day18 str = (show $ part1 inp, show $ part2 inp) where inp = parse str
