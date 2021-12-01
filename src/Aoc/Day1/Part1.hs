module Aoc.Day1.Part1 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Maybe

solve :: [BS.ByteString] -> String
solve = show . solve' . fmap (fst . fromJust . BC.readInt)

solve' :: [Int] -> Int
solve' [] = 0
solve' [_] = 0
solve' (i:is)
  | i < head is  = 1 + solve' is
  | otherwise    = solve' is
