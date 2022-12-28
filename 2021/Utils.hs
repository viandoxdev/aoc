module Utils where
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed

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
