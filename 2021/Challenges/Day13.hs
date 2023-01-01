module Challenges.Day13 where
import Utils
import Debug.Trace
import Data.Array (accumArray, elems)
import Data.List (intercalate, transpose)
import BigLetters

data Fold = Vert Int | Horz Int deriving(Show)
type Vec2 = (Int, Int)

parse :: String -> ([Vec2], [Fold])
parse str = (map v2 points, map fld folds)
    where [points, folds] = map lines $ splitOn "\n\n" str
          v2 l = (read x, read y) where [x, y] = splitOn "," l
          fld l = if 'x' `elem` p then Vert (read x) else Horz (read x) where [p,x] = splitOn "=" l

fold :: Fold -> [Vec2] -> [Vec2]
fold (Vert fx) = map (\(x, y) -> (fx - abs (fx - x), y)) . filter (\(x, _) -> x /= fx)
fold (Horz fy) = map (\(x, y) -> (x, fy - abs (fy - y))) . filter (\(_, y) -> y /= fy)

bbox :: [Vec2] -> (Vec2, Vec2)
bbox vs = ((minimum $ map fst vs, minimum $ map snd vs), (maximum $ map fst vs, maximum $ map snd vs))

height :: (Vec2, Vec2) -> Int
height ((_, y0), (_, y1)) = y1 - y0 + 1

showg :: [Vec2] -> String
showg l = unlines (transpose $ chunks (height box) $ elems arr)
    where box = bbox l
          arr = accumArray (const id) ' ' box $ map (\(x, y) -> ((x, y), '#')) l

traceShowIdg :: [Vec2] -> [Vec2]
traceShowIdg l = trace (showg l) l

part1 :: ([Vec2], [Fold]) -> Int
part1 (l, fs) = length $ unique $ fold (head fs) l

part2 :: ([Vec2], [Fold]) -> String
part2 (l, fs) = readString '#' $ showg $ foldl (flip fold) l fs

day13 :: String -> (String, String)
day13 str = (show $ part1 inp, part2 inp) where inp = parse str
