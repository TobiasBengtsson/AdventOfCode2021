module Aoc.Day3.Part1 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Control.Lens.Lens
import Data.Char (digitToInt)
import Data.Maybe (fromJust, listToMaybe)
import Numeric (readInt)

solve :: [BS.ByteString] -> String
solve = answer . (??) [fmap flipBit, id] . mcbString

answer :: [String] -> String
answer = show . product . fmap binToDec

-- Returns a bitstring with the most common bit of each column
mcbString :: [BS.ByteString] -> String
mcbString = fmap mostCommonBit . fmap BC.unpack . BC.transpose

mostCommonBit :: String -> Char
mostCommonBit allBits
  | cnt '1' >= cnt '0' = '1'
  | otherwise          = '0'
  where
    cnt bit = elemCount bit allBits

elemCount :: Eq a => a -> [a] -> Int
elemCount e = length . filter (== e)

flipBit :: Char -> Char
flipBit '1' = '0'
flipBit '0' = '1'

binToDec :: Integral a => String -> a
binToDec = fromJust . fmap fst . listToMaybe . readInt 2 (`elem` ['0','1']) digitToInt
