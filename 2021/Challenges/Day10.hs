module Challenges.Day10 (day10) where
import Control.Monad (foldM)
import Utils (leftToMaybe, median)
import Data.Either (fromRight, isLeft)
import Data.Maybe (mapMaybe)

data BracketKind = Round | Square | Curly | Angled deriving(Eq)
data Bracket = Opening BracketKind | Closing BracketKind

parse :: String -> [[Bracket]]
parse = map (map brckt) . lines where
    brckt '(' = Opening Round;  brckt ')' = Closing Round
    brckt '[' = Opening Square; brckt ']' = Closing Square
    brckt '{' = Opening Curly;  brckt '}' = Closing Curly
    brckt '<' = Opening Angled; brckt '>' = Closing Angled
    brckt _ = error "Bad input"

errorScore :: BracketKind -> Int
errorScore Round = 3
errorScore Square = 57
errorScore Curly = 1197
errorScore Angled = 25137

complScore :: BracketKind -> Int
complScore Round = 1
complScore Square = 2
complScore Curly = 3
complScore Angled = 4

completionScore :: [BracketKind] -> Int
completionScore = foldl (\a e -> a * 5 + complScore e) 0

buildStack :: [Bracket] -> Either BracketKind [BracketKind]
buildStack = foldM fld [] where
    fld stack (Opening kind) = Right (kind : stack)
    fld (x:xs) (Closing kind) | x == kind = Right xs
                              | otherwise = Left kind
    fld [] (Closing kind) = Left kind

hasError :: [Bracket] -> Bool
hasError = isLeft . buildStack

findError :: [Bracket] -> Maybe BracketKind
findError = leftToMaybe . buildStack

complete :: [Bracket] -> [BracketKind]
complete = fromRight [] . buildStack

part1 :: [[Bracket]] -> Int
part1 = sum . mapMaybe (fmap errorScore . findError)

part2 :: [[Bracket]] -> Int
part2 = median . map (completionScore . complete) . filter (not . hasError)

day10 :: String -> (String, String)
day10 str = (show $ part1 inp, show $ part2 inp) where inp = parse str
