module Aoc.Day9.Part2 where

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort)

import Aoc.Day9.Part1

solve :: [BS.ByteString] -> String
solve s = show $ product $ take 3 $ reverse $ sort $ map Map.size $ basinsFromSinks (allSinks g) g
  where
    g = readGrid s

basinsFromSinks :: PartialGrid -> Grid -> [PartialGrid]
basinsFromSinks sink g = Map.elems $ Map.mapWithKey (\k _ -> searchBasin k g) sink

searchBasin :: Coord -> Grid -> PartialGrid
searchBasin c = searchBasin' [c] Set.empty Map.empty

searchBasin' :: [Coord] -> Set.Set Coord -> PartialGrid -> Grid -> PartialGrid
searchBasin' [] _ b _ = b
searchBasin' (c:candidates) visited basin g =
  case height c g of
    Nothing -> searchBasin' candidates newVisited basin g
    Just 9  -> searchBasin' candidates newVisited basin g
    Just x  -> searchBasin' newCandidates newVisited (newBasin x) g
  where
    newCandidates = candidates ++ filter (\c' -> not $ Set.member c' visited) (surroundingPoints c)
    newVisited = Set.insert c visited
    newBasin h = Map.insert c h basin
