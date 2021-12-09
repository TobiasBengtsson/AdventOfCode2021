module Aoc.Day9.Part1 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Char (digitToInt)

type Coord = (Int, Int)
type Grid = Map.Map Coord Int
type PartialGrid = Grid

solve :: [BS.ByteString] -> String
solve = show . sum . map riskFn . allSinkHeights . readGrid

riskFn :: Int -> Int
riskFn = (+ 1)

allSinkHeights :: Grid -> [Int]
allSinkHeights = Map.elems . allSinks

allSinks :: Grid -> PartialGrid
allSinks g = Map.filterWithKey (\k v -> isSink k g) g

isSink :: Coord -> Grid -> Bool
isSink p g = all (isLowerThan thisHeight) (surroundingHeights p g)
  where
    thisHeight = fromJust $ height p g

surroundingHeights :: Coord -> Grid -> [Maybe Int]
surroundingHeights (x,y) g = map (`height` g) [(x+1, y), (x, y+1), (x-1, y), (x, y-1)]

height :: Coord -> Grid -> Maybe Int
height = Map.lookup

isLowerThan :: Int -> Maybe Int -> Bool
isLowerThan p1 p2 = case p2 of
  Nothing -> True
  Just x  -> p1 < x

readGrid :: [BS.ByteString] -> Grid
readGrid s = snd $ foldl (\(j,grid) s -> (j+1, Map.union grid $ readRow j s)) (0, Map.empty) s

readRow :: Int -> BS.ByteString -> Grid
readRow i = Map.fromList . zip [(i,x) | x <- [0..]] . map digitToInt . BC.unpack
