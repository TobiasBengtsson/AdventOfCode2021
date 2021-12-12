module Aoc.Day12.Part2 where

import qualified Data.ByteString as BS
import Data.List

import qualified Aoc.Day12.Part1 as P1

solve :: [BS.ByteString] -> String
solve = show . length . allPaths "start" "end" . map P1.readRow

allPaths :: String -> String -> [(String, String)] -> [[String]]
allPaths start end edges = allPaths' start end edges []

allPaths' :: String -> String -> [P1.Edge] -> P1.Path -> [P1.Path]
allPaths' start end edges visited
  | start == end = [end:visited]
  | start == "start" && not (null visited) = []
  | P1.isSmallNode start &&
    (start `elem` visited) &&
    (length (nub $ filter P1.isSmallNode visited) /= length (filter P1.isSmallNode visited)) = []
  | otherwise = concatMap (\s -> allPaths' s end edges (start:visited)) $ P1.adjNodes start edges
