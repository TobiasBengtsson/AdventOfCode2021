module Aoc.Day19.Part2 where

import qualified Data.ByteString as BS
import qualified Data.Set as Set
import Data.Maybe
import Control.Monad
import Control.Arrow

import Aoc.Day19.Part1

singleton a = [a]

solve :: [BS.ByteString] -> String
solve = readScanners >>> (singleton . assumeOrigin . head) &&& tail >>> uncurry solve' >>> maxDistance >>> show

maxDistance :: [ScannerState] -> Int
maxDistance = map location >>> id &&& id >>> uncurry (liftM2 distance) >>> maximum

distance (x,y,z) (x',y',z') = abs (x-x') + abs (y-y') + abs (z-z')
