module BigLetters (readString) where

import Language.Haskell.TH.Syntax (lift)
import Data.Word (Word32)
import Data.List (transpose, foldl', elemIndex)
import Utils (splitOn)

-- Module used to parse the "Big letters solutions in aoc"
-- i.e
-- #  # #  # #  #   ##  ##   ##    ## ####
-- #  # # #  #  #    # #  # #  #    #    #
-- #### ##   #  #    # #    #  #    #   # 
-- #  # # #  #  #    # # ## ####    #  #   
-- #  # # #  #  # #  # #  # #  # #  # #   
-- #  # #  #  ##   ##   ### #  #  ##  ####
--
-- becomes "HKUJGAJZ"

-- letters are 6x3, 6x4 or 6x5 bitmaps (all fit in a Word32)

-- letters, I added D,M,N,Q,T,V,W and X but these shouldn't ever appear as a puzzle solution
letters :: [Letter]
letters = parseString '#' $ unlines -- evaluate at compile time because why not.
    [ " ##  ###   ##  ###  #### ####  ##  #  # ###   ## #  # #    #   # #  #  ##  ###   ##   ###   ### ### #  # #   # #   # #   # #   # ####"
    , "#  # #  # #  # #  # #    #    #  # #  #  #     # # #  #    ## ## ## # #  # #  # #  #  #  # #     #  #  # #   # #   #  # #  #   #    #"
    , "#  # ###  #    #  # ###  ###  #    ####  #     # ##   #    # # # # ## #  # #  # #  #  #  # #     #  #  # #   # #   #   #    # #    # "
    , "#### #  # #    #  # #    #    # ## #  #  #     # # #  #    #   # #  # #  # ###  #  #  ###   ##   #  #  # #   # # # #  # #    #    #  "
    , "#  # #  # #  # #  # #    #    #  # #  #  #  #  # # #  #    #   # #  # #  # #    # ##  # #     #  #  #  #  # #  ## ## #   #   #   #   "
    , "#  # ###   ##  ###  #### #     ### #  # ###  ##  #  # #### #   # #  #  ##  #     # ## #  # ###   #   ##    #   #   # #   #   #   ####" ]

type Letter = Word32

parseString :: Char -> String -> [Letter]
parseString t = map (bitsToLetter . concat . pad . map (map (==t))) . splitOn ["      "] . transpose . lines

bitsToLetter :: [Bool] -> Letter
bitsToLetter = fromIntegral . foldl' (\a e -> a * 2 + (if e then 1 else 0)) 0

pad :: [[Bool]] -> [[Bool]]
pad l | len <= 5 = l ++ replicate (5 - len) (replicate 6 False)
      | otherwise = error "Can't have a letter larger than 5 colmns"
    where len = length l

readString :: Char -> String -> String
readString c = map (maybe '?' (['A'..'Z']!!) . (`elemIndex` letters)) . parseString c
