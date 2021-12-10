module Aoc.Day9.Part1 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as Map
import Control.Monad.Reader
import Control.Arrow
import Data.Bifunctor
import Data.Maybe
import Data.Char (digitToInt)
import Data.Function.Slip

type Coord = (Int, Int)
type Grid = Map.Map Coord Int
type PartialGrid = Grid

solve :: [BS.ByteString] -> String
solve = show . sum . map riskFn . allSinkHeights . readGrid

riskFn :: Int -> Int
riskFn = (+ 1)

allSinkHeights :: PartialGrid -> [Int]
allSinkHeights = Map.elems . allSinks

allSinks :: Grid -> PartialGrid
allSinks = Map.filterWithKey =<< slipl isSink

isSink :: Coord -> Int -> Grid -> Bool
isSink p h g = all (isHigherThan h) (surroundingHeights p g)

surroundingHeights :: Coord -> Grid -> [Maybe Int]
surroundingHeights = curry $ surroundingPoints *** flip height >>> uncurry (flip map)

surroundingPoints :: Coord -> [Coord]
surroundingPoints (x,y) = [(x+1, y), (x, y+1), (x-1, y), (x, y-1)]

height :: Coord -> Grid -> Maybe Int
height = Map.lookup

isHigherThan :: Int -> Maybe Int -> Bool
isHigherThan p1 Nothing  = True
isHigherThan p1 (Just x) = p1 < x

readGrid :: [BS.ByteString] -> Grid
readGrid = foldl (curry $ Map.union *** uncurry readRow >>> uncurry ($)) Map.empty . zip [0..]

readRow :: Int -> BS.ByteString -> PartialGrid
readRow i = Map.fromList . zip [(i,x) | x <- [0..]] . map digitToInt . BC.unpack
