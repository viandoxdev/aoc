module Utils where
import Control.Applicative (ZipList(getZipList, ZipList))
import Data.List (tails, unfoldr, sort)

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn _ [] = []
splitOn p l =
    if and ((length z == pl) : z)
        then [] : splitOn p (drop pl l)
        else addToHead (head l) (splitOn p $ tail l)
    where z = zipWith (==) p l
          pl = length p
          addToHead v (h:tl) = (v : h) : tl
          addToHead v [] = [[v]]

transpose' :: [[a]] -> [[a]]
transpose' = getZipList . traverse ZipList

windows :: Int -> [a] -> [[a]]
windows n = transpose' . take n . tails

chunks :: Int -> [a] -> [[a]]
chunks n = takeWhile (not . null) . unfoldr (Just . splitAt n)

leftToMaybe :: Either a b -> Maybe a
leftToMaybe = either Just (const Nothing)

median :: Ord a => [a] -> a
median list = sort list !! i where i = length list `div` 2
