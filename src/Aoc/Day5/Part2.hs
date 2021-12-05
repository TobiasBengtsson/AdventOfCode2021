module Aoc.Day5.Part2 where

import qualified Data.ByteString as BS
import qualified Data.MultiSet as MultiSet

import qualified Aoc.Day5.Part1 as P1

solve :: [BS.ByteString] -> String
solve = show . P1.countMultiple . MultiSet.unions . fmap (MultiSet.fromList . P1.traceLine . P1.readLine)
