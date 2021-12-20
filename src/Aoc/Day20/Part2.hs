module Aoc.Day20.Part2 where

import qualified Data.ByteString as BS
import Aoc.Day20.Part1

solve :: [BS.ByteString] -> String
solve = solve' 50
