module Aoc.Day13.Part2 where

import qualified Data.ByteString as BS
import qualified Data.Set as Set
import Data.List (break)

import Aoc.Day13.Part1

solve :: [BS.ByteString] -> String
solve s = show $ render sln
  where
    parts = break (== "") s
    dots  = readDots $ fst parts
    folds = readFolds $ drop 1 $ snd parts
    sln   = foldl (flip fold) (Set.fromList dots) folds

render sln = snd $ foldl (\(y,s) r -> (y+1, s ++ "\n" ++ map (\x -> toChar (x,y)) r)) (0,"") grid
  where
    maxX  = foldr (\(x,y) m -> max x m) 0 sln
    maxY  = foldr (\(x,y) m -> max y m) 0 sln
    grid  = replicate (maxY + 1) [0..maxX]
    toChar (x,y) = if (x,y) `elem` sln then '#' else '.'
