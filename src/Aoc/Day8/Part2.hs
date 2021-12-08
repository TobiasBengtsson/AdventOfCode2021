module Aoc.Day8.Part2 where

import qualified Data.ByteString as BS

import qualified Aoc.Day8.Part1 as P1

solve :: [BS.ByteString] -> String
solve = show . sum . map (read . P1.solveRow)
