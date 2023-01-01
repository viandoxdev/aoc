module Challenges.Day08 (day08) where
import Utils (splitOn)
import Data.Array
import Data.List ((\\), intersect, sort)

-- | A segment 
type Segment = Char
-- | A segment we know
-- |  00
-- | 1  2
-- |  33
-- | 4  5
-- |  66
type RealSegment = Int
type Digit = [Segment]
type RealDigit = [RealSegment]
-- | The possibles segments a RealSegment might be mapped to
type Options = [Segment]
-- | What we know of the mapping from Segment to RealSegment
type State = Array RealSegment [Segment]
-- | A mapping from Segment to RealSegment
type Mapping = Array Segment RealSegment

updateOptions :: Digit -> RealDigit -> (RealSegment, Options) -> (RealSegment, Options)
updateOptions digit rdigit (rseg, opts) | rseg `elem` rdigit = (rseg, opts `intersect` digit)
                                        | otherwise = (rseg, opts \\ digit)
-- | returns either [] or [state]
updateState :: State -> Digit -> RealDigit -> [State]
updateState state digit rdigit = filter validateState [accumArray (++) [] (0,6) $ map (updateOptions digit rdigit) $ assocs state | valid]
    where valid = not $ any ((null . intersect digit) . (state!)) rdigit

validateState :: State -> Bool
validateState state = not $ any (null . (state!)) [0..6]

defaultState :: State
defaultState = array (0,6) [(i, ['a'..'g']) | i <- [0..6]]

realDigits :: Array Int RealDigit
realDigits = listArray (0,9) [ 
           [0,1,2,4,5,6]
         , [2,5]
         , [0,2,3,4,6]
         , [0,2,3,5,6]
         , [1,2,3,5]
         , [0,1,3,5,6]
         , [0,1,3,4,5,6]
         , [0,2,5]
         , [0,1,2,3,4,5,6]
         , [0,1,2,3,5,6] ]

realDigitInt :: RealDigit -> Int
realDigitInt [0,1,2,4,5,6] = 0
realDigitInt [2,5] = 1
realDigitInt [0,2,3,4,6] = 2
realDigitInt [0,2,3,5,6] = 3
realDigitInt [1,2,3,5] = 4
realDigitInt [0,1,3,5,6] = 5
realDigitInt [0,1,3,4,5,6] = 6
realDigitInt [0,2,5] = 7
realDigitInt [0,1,2,3,4,5,6] = 8
realDigitInt [0,1,2,3,5,6] = 9
realDigitInt _ = error "Invalid digit"

processDigit :: Digit -> State -> [State]
processDigit digit state = case length digit of
    0 -> error "Can't have a zero segment digit"
    1 -> error "Can't have a one segment digit"
    2 -> u 1
    3 -> u 7
    4 -> u 4
    5 -> [2,3,5] >>= u
    6 -> [0,6,9] >>= u
    7 -> [state] -- A 7 segment digit doesn't give any information
    _ -> error "Can't have a digit composed of more than 7 segments"
    where u = updateState state digit . (realDigits!)

foldProcess :: [State] -> Digit -> [State]
foldProcess states digit = (>>= processDigit digit) states

stateMapping :: State -> Mapping
stateMapping state = array ('a','g') $ map f $ assocs state where
    f (rseg, h:_) = (h, rseg)
    f (_, []) = error "Invalid state can't be a mapping"

getMapping :: [Digit] -> Mapping
getMapping = stateMapping . head . foldl foldProcess [defaultState]

getDigit :: Mapping -> Digit -> Int
getDigit m = realDigitInt . sort . map (m!)

getValue :: Mapping -> [Digit] -> Int
getValue m = foldl f 0 . map (getDigit m) where
    f a e = a * 10 + e

parse :: String -> [([Digit], [Digit])]
parse = map (tuple . map words . splitOn " | ") . lines where
    tuple [a, b] = (a, b)
    tuple _ = error "Bad input"

unique :: Int -> Bool
unique = (`elem` [2,3,4,7])

part1 :: [([Digit], [Digit])] -> Int
part1 = length . filter (unique . length) . (>>= snd)

part2 :: [([Digit], [Digit])] -> Int
part2 = sum . map f where
    f (i, o) = getValue (getMapping i) o

day08 :: String -> (String, String)
day08 str = (show $ part1 inp, show $ part2 inp) where inp = parse str
