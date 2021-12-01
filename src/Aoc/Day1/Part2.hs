module Aoc.Day1.Part2 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Maybe

solve :: [BS.ByteString] -> String
solve lines = show $ solve' $ fmap (fst . fromJust . BC.readInt) lines

solve' :: [Int] -> Int
solve' [] = 0
solve' [_] = 0
solve' [_, _] = 0
solve' [_, _, _] = 0
solve' (i:is)
  | fstWindow < sndWindow = 1 + solve' is
  | otherwise             = solve' is
  where
    fstWindow = sum (i:(take 2 is))
    sndWindow = sum (take 3 is)
