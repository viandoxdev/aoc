module Challenges.Day3 where
import Data.List (transpose)

bitBool :: Char -> Bool
bitBool '0' = False
bitBool '1' = True
bitBool _ = error "Bad input"

dec :: [Bool] -> Int
dec = fst . foldr fold (0, 1) where
    fold True (a, f) = (a + f, f * 2)
    fold False (a, f) = (a, f * 2)

parse :: String -> [[Bool]]
parse = map (map bitBool) . lines

select :: ((Int, Int) -> Bool) -> [Bool] -> Bool
select sel = sel . foldl count (0, 0) where
    count (t, f) True = (t + 1, f)
    count (t, f) False = (t, f + 1)

part1 :: [[Bool]] -> Int
part1 inp = dec bits * dec (map not bits)
    where bits = map (select $ uncurry (<)) $ transpose inp

part2' :: ((Int, Int) -> Bool) -> Int -> [[Bool]] -> Int
part2' _ _ [n] = dec n
part2' s i l = part2' s (i + 1) $ filter ((== bit) . (!!i)) l where bit = select s $ transpose l !! i

part2 :: [[Bool]] -> Int
part2 inp = part2' (uncurry (<)) 0 inp * part2' (uncurry (>=)) 0 inp

day3 :: String -> (String, String)
day3 str = (show $ part1 input, show $ part2 input) where input = parse str
