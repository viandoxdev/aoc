module Challenges.Day02 (day02) where

newtype Vec2 = Vec2 (Int, Int)

instance Num Vec2 where
    (+) (Vec2 (a, b)) (Vec2 (c, d)) = Vec2 (a + c, b + d)
    (-) (Vec2 (a, b)) (Vec2 (c, d)) = Vec2 (a - c, b - d)
    (*) (Vec2 (a, b)) (Vec2 (c, d)) = Vec2 (a * c, b * d)
    abs (Vec2 (a, b)) = Vec2 (abs a, abs b)
    signum (Vec2 (a, b)) = Vec2 (signum a, signum b)
    fromInteger i = Vec2 (fromInteger i, fromInteger i)

mul :: Vec2 -> Int
mul (Vec2 (x, y)) = x * y

data Com = Up Int | Down Int | Fwd Int

parse :: String -> [Com]
parse = map (f . words) . lines where
    f [a, b] | a == "forward" = Fwd (read b)
        | a == "up" = Up (read b)
        | a == "down" = Down (read b)
    f w = error $ "Bad input " ++ show w

part1 :: [Com] -> Int
part1 = mul . sum . map offset where
    offset (Up s) = Vec2 (0, -s)
    offset (Down s) = Vec2 (0, s)
    offset (Fwd s) = Vec2 (s, 0)

part2 :: [Com] -> Int
part2 = mul . snd . foldl f (0, Vec2 (0, 0)) where
    f (aim, pos) (Up s) = (aim - s, pos)
    f (aim, pos) (Down s) = (aim + s, pos)
    f (aim, Vec2 (x, y)) (Fwd s) = (aim, Vec2 (x + s, y + aim * s))

day02 :: String -> (String, String)
day02 str = (show $ part1 input, show $ part2 input)
    where input = parse str
