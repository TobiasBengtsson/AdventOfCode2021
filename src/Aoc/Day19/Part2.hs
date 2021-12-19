module Aoc.Day19.Part2 where

import qualified Data.ByteString as BS
import qualified Data.Set as Set
import Data.Maybe
import Control.Monad

import Aoc.Day19.Part1

solve :: [BS.ByteString] -> String
solve bs = show $ maxDistance $ solve' initScanners Set.empty
  where
    scanners = readScanners bs
    initScanners = (head scanners) { location = Just (0,0,0) } : tail scanners

maxDistance :: [ScannerData] -> Int
maxDistance sds = maximum $ map (\(a,b) -> distance a b) combinations
  where
    combinations = liftM2 (,) (map (fromJust . location) sds) (map (fromJust . location) sds)

distance (x,y,z) (x',y',z') = abs (x-x') + abs (y-y') + abs (z-z')
