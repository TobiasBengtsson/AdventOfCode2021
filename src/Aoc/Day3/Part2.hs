module Aoc.Day3.Part2 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.List
import qualified Aoc.Day3.Part1 as P1

solve :: [BS.ByteString] -> String
solve lines = show $ o2genRating * co2ScrubRating
  where
    o2genRating = P1.binToDec $ solve' True 0 $ fmap BC.unpack lines
    co2ScrubRating = P1.binToDec $ solve' False 0 $ fmap BC.unpack lines

solve' :: Bool -> Int -> [String] -> String
solve' _ _ [] = error "No match found"
solve' _ _ [x] = x
solve' b i xs = solve' b (i + 1) $ filter meetCriteria xs
  where
    mcb = P1.mostCommonBit ((transpose xs) !! i)
    meetCriteria line = case b of
      True ->  line !! i == mcb
      False -> line !! i /= mcb
