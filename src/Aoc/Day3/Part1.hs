module Aoc.Day3.Part1 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (digitToInt)
import Data.Maybe
import Numeric

solve :: [BS.ByteString] -> String
solve lines = show $ (binToDec lcbString) * (binToDec $ mcbString lines)
  where
    lcbString = fmap (\x -> if x == '1' then '0' else '1') $ mcbString lines

mcbString :: [BS.ByteString] -> String
mcbString = fmap mostCommonBit . fmap BC.unpack . BC.transpose

mostCommonBit :: String -> Char
mostCommonBit allBits
  | bitCount >= zeroCount = '1'
  | otherwise             = '0'
  where
    bitCount = length (filter (== '1') allBits)
    zeroCount = length allBits - bitCount

binToDec :: Integral a => String -> a
binToDec = fromJust . fmap fst . listToMaybe . readInt 2 (`elem` ['0','1']) digitToInt
