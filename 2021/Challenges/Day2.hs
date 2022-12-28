module Challenges.Day2 where

instance (Num a, Num b) => Num (a, b) where
    (+) (a, b) (c, d) = (a + c, b + d)
    (-) (a, b) (c, d) = (a - c, b - d)
    (*) (a, b) (c, d) = (a * c, b * d)
    abs (a, b) = (abs a, abs b)
    signum (a, b) = (signum a, signum b)
    fromInteger i = (fromInteger i, fromInteger i)

mul :: (Int, Int) -> Int
mul (x, y) = x * y

data Com = Up Int | Down Int | Fwd Int

parse :: String -> [Com]
parse = map (f . words) . lines where
    f [a, b] | a == "forward" = Fwd (read b)
        | a == "up" = Up (read b)
        | a == "down" = Down (read b)
    f w = error $ "Bad input " ++ show w

part1 :: [Com] -> Int
part1 = mul . sum . map offset where
    offset (Up s) = (0, -s)
    offset (Down s) = (0, s)
    offset (Fwd s) = (s, 0)

part2 :: [Com] -> Int
part2 = mul . snd . foldl f (0, (0, 0)) where
    f (aim, pos) (Up s) = (aim - s, pos)
    f (aim, pos) (Down s) = (aim + s, pos)
    f (aim, (x, y)) (Fwd s) = (aim, (x + s, y + aim * s))

day2 :: String -> (String, String)
day2 str = (show $ part1 input, show $ part2 input)
    where input = parse str
