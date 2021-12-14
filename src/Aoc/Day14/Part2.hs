module Aoc.Day14.Part2 where

import qualified Data.ByteString as BS
import Aoc.Day14.Part1

solve :: [BS.ByteString] -> String
solve = solve' 40
