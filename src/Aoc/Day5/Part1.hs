module Aoc.Day5.Part1 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Maybe as Maybe
import qualified Data.MultiSet as MultiSet

type Point = (Int, Int)
type Line = (Point, Point)

readLine :: BS.ByteString -> Line
readLine s = (readPoint (head splitStr), readPoint (last splitStr))
  where
    splitStr = BC.split ' ' s

readPoint :: BS.ByteString -> Point
readPoint s = (toInt (head splitStr), toInt (last splitStr))
  where
    splitStr = BC.split ',' s

solve :: [BS.ByteString] -> String
solve = show . countMultiple . MultiSet.unions . fmap (MultiSet.fromList . traceLine) . filter straight . fmap readLine

countMultiple :: MultiSet.MultiSet a -> Int
countMultiple = MultiSet.foldOccur (\p occ x -> if occ < 2 then x else x + 1) 0

straight :: Line -> Bool
straight ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

traceLine :: Line -> [Point]
traceLine ((x1, y1), (x2, y2)) =
    [(x,y)
      | x <- [x1,x1 + signum (x2 - x1)..x2]
      | y <- [y1,y1 + signum (y2 - y1)..y2]]

toInt :: BS.ByteString -> Int
toInt = fst . Maybe.fromJust . BC.readInt
