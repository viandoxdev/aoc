{-# LANGUAGE TupleSections #-}
module Challenges.Day14 (day14) where
import qualified HashMap as H
import HashMap ((!!!))
import Utils ( splitOn, windows )
import Data.List (sort, foldl', groupBy, sortOn)
import Data.Array (Array, array, accumArray, assocs, (!), bounds)
import Data.Tuple (swap)

type Subs = Array Int [Int]
type Pairs = Array Int Int
type Strings = Array Int String

registry :: String -> H.Map String Int
registry = foldl' fld (H.new []) . map (head . words) . lines where
    fld m c = H.insert m [(c, H.used m)]

parse :: String -> ((Char, Char), Pairs, Subs, Strings)
parse str = ( (head form, last form)
            , accumArray (+) 0 bds $ map ((,1) . (reg!!!)) $ windows 2 form
            , array bds $ map (f . words) $ lines subst
            , array bds $ map swap $ H.toList reg )
    where [form, subst] = splitOn "\n\n" str
          reg = registry subst
          bds = (0, H.used reg - 1)
          f [[a,b], _, [c]] = (reg !!! [a,b], [reg !!! [a,c], reg !!! [c,b]])
          f _ = error "Bad input"

step :: Subs -> Pairs -> Pairs
step s p = accumArray (+) 0 (bounds p) $ (concatMap (\(i, a) -> map (,a) $ s!i) . assocs) p

result :: Strings -> (Char, Char) -> Pairs -> Int
result strs (f, l) = (\xs -> last xs - head xs)
                   . sort . map ((`div` 2) . sum . map snd)
                   . groupBy (\(a,_) (b,_) -> a == b) . sortOn fst
                   . (++[(f,1),(l,1)])
                   . concatMap (\(i,a) -> map (,a) $ strs!i)
                   . assocs

solve :: Int -> ((Char, Char), Pairs, Subs, Strings) -> Int
solve steps (ends, p, s, strs) = result strs ends $ (!!steps) $ iterate (step s) p

day14 :: String -> (String, String)
day14 str = (show $ solve 10 inp, show $ solve 40 inp) where inp = parse str
