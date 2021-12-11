module Aoc.Day11.Part2 where

import qualified Data.ByteString as BS
import qualified Data.Map as Map

import Aoc.Day11.Part1

solve :: [BS.ByteString] -> String
solve = show . findSync 1 . readGrid

findSync :: Int -> Grid -> Int
findSync i g = if fst g' == Map.size g then i else findSync (i+1) $ snd g'
  where
    g' = step g
