module Aoc.Day6.Part2 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.MultiSet as MS

import qualified Aoc.Day6.Part1 as P1

solve :: [BS.ByteString] -> String
solve = show . MS.size . (!! 256) . iterate P1.iter . P1.readDays . head
