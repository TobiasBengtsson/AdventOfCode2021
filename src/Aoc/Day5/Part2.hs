module Aoc.Day5.Part2 where

import qualified Data.ByteString as BS
import qualified Data.MultiSet as MultiSet

import qualified Aoc.Day5.Part1 as P1

solve :: [BS.ByteString] -> String
solve = show . MultiSet.foldOccur (\p occ x -> if occ < 2 then x else x + 1) 0 . MultiSet.unions . fmap (MultiSet.fromList . traceLine . P1.readLine)

traceLine :: P1.Line -> [P1.Point]
traceLine ((x1, y1), (x2, y2))
  | x1 == x2 && y1 <  y2 = fmap (x1, ) [y1..y2]
  | x1 == x2             = fmap (x1, ) [y2..y1]
  | x1 <  x2 && y1 == y2 = fmap (, y1) [x1..x2]
  |             y1 == y2 = fmap (, y1) [x2..x1]
  | x1 <  x2 && y1 <  y2 = [(x,y) | x <- [x1..x2], y <- [y1..y2], y - x == y1 - x1]
  | x1 <  x2             = [(x,y) | x <- [x1..x2], y <- [y2..y1], y + x == y1 + x1]
  | x1 >  x2 && y1 <  y2 = [(x,y) | x <- [x2..x1], y <- [y1..y2], y + x == y1 + x1]
  | otherwise            = [(x,y) | x <- [x2..x1], y <- [y2..y1], y - x == y1 - x1]
