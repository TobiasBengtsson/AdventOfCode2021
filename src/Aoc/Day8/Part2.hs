module Aoc.Day8.Part2 where

import qualified Data.ByteString as BS

import qualified Aoc.Day8.Part1 as P1

solve :: [BS.ByteString] -> String
solve = show . sum . map solveRow

solveRow :: BS.ByteString -> Int
solveRow s = read $ concatMap (show . P1.display . (`P1.mapSignal` charmap)) output
  where
    obs = fst $ P1.readRow s
    output = snd $ P1.readRow s
    charmap = P1.reduceCompleted $ P1.filterCharCounts obs $ foldr P1.filterEasy P1.allMappings obs
