module Aoc.Day3.Part2 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Control.Lens.Lens
import Data.List

import qualified Aoc.Day3.Part1 as P1

data BitCriteria = MCB | LCB

solve :: [BS.ByteString] -> String
solve = P1.answer . (??) (fmap solve' [MCB, LCB]) . fmap BC.unpack

solve' :: BitCriteria -> [String] -> String
solve' bc = search bc 0

search :: BitCriteria -> Int -> [String] -> String
search _ _ [] = error "No match found"
search _ _ [x] = x
search b i xs = search b (i + 1) $ filter meetCriteria xs
  where
    mcb = P1.mostCommonBit ((transpose xs) !! i)
    meetCriteria line = operator b (line !! i) mcb
    operator MCB = (==)
    operator LCB = (/=)
