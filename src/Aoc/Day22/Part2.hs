module Aoc.Day22.Part2 where

import qualified Data.ByteString.Char8 as BC
import Aoc.Day22.Part1

solve :: [BC.ByteString] -> String
solve =  solve' . readPart2

readPart2 :: [BC.ByteString] -> [CuboidAction]
readPart2 = map readLine
