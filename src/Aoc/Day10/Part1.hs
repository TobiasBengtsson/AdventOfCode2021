module Aoc.Day10.Part1 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Either (lefts)
import Data.Maybe (fromMaybe)

solve :: [BS.ByteString] -> String
solve = show . sum . map corruptScore . lefts . map ((`parse` []) . BC.unpack)

data ParseError = Corrupt Char | Incomplete String

isCorrupt :: ParseError -> Bool
isCorrupt (Corrupt _) = True
isCorrupt _           = False

corruptScore :: ParseError -> Int
corruptScore (Corrupt x) = fromMaybe 0 $ lookup x (zip closings [3, 57, 1197, 25137])
corruptScore _ = 0

parse :: String -> String -> Either ParseError ()
parse [] [] = Right ()
parse [] s  = Left $ Incomplete s
parse (x:xs) state
  | x `elem` openings = parse xs (x:state)
  | x `elem` closings = if not $ matches $ head state
                      then Left (Corrupt x)
                      else parse xs (drop 1 state)
  | otherwise = error "Unrecognized character"
  where
    matches o = Just x == lookup o (zip openings closings)

openings = ['(', '[', '{', '<']
closings = [')', ']', '}', '>']
