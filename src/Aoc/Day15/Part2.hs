module Aoc.Day15.Part2 where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

import Aoc.Day15.Part1

solve :: [BS.ByteString] -> String
solve = show . runSearch . expandGrid . readGrid

expandGrid :: Grid -> Grid
expandGrid g = Map.unions $ map (\i -> addY (origYSize * i) $ inc i expandedX) [0..4]
  where
    origMax = maximum $ Map.keys g
    origXSize = fst origMax + 1
    origYSize = snd origMax + 1
    expandedX = Map.unions $ map (\i -> addX (origXSize * i) $ inc i g) [0..4]

inc :: Int -> Grid -> Grid
inc 0 = id
inc 1 = Map.map (\v -> if v == 9 then 1 else v + 1)
inc i = inc 1 . inc (i-1)

addX :: Int -> Grid -> Grid
addX dx = Map.mapKeys (\(x,y) -> (x+dx,y))

addY :: Int -> Grid -> Grid
addY dy = Map.mapKeys (\(x,y) -> (x,y+dy))
