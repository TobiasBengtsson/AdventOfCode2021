module Aoc.Day19.Part1 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Control.Monad (liftM2)
import Data.Either
import Data.Function
import Data.Maybe
import Data.List.Split (splitOn)
import Data.List (intersect, maximumBy, nub, transpose)
import qualified Data.Set as Set

type Coord = (Int, Int, Int)
data ScannerData = ScannerData
  { readings :: [Coord] }

data ScannerState = ScannerState
  { beacons :: [Coord]
  , location :: Coord }

solve :: [BS.ByteString] -> String
solve bs = show $ beaconCount $ solve' [assumeOrigin (head scanners)] $ tail scanners
  where
    scanners = readScanners bs

assumeOrigin :: ScannerData -> ScannerState
assumeOrigin sd = ScannerState { beacons = readings sd, location = (0,0,0) }

beaconCount :: [ScannerState] -> Int
beaconCount = length . nub . concatMap beacons

solve' :: [ScannerState] -> [ScannerData] -> [ScannerState]
solve' sss [] = sss
solve' sss sds = solve' (tail sss ++ solved ++ [pivot]) unsolved
  where
    solved = rights maybeSolved
    unsolved = lefts maybeSolved
    maybeSolved = map (solveWithPivot pivot) sds
    pivot = head sss

solveWithPivot :: ScannerState -> ScannerData -> Either ScannerData ScannerState
solveWithPivot pivot sd
  | fst ol >= 12 = Right $ snd ol
  | otherwise    = Left sd
  where
    ol = allOverlaps (beacons pivot) (readings sd)

allOverlaps :: [Coord] -> [Coord] -> (Int, ScannerState)
allOverlaps cs1 cs2 = maximumBy (compare `on` fst) $ map (fixedOrientationOverlaps cs1) orientations
  where
    orientations = transpose $ map allOrientations cs2

fixedOrientationOverlaps :: [Coord] -> [Coord] -> (Int, ScannerState)
fixedOrientationOverlaps cs1 cs2 = maximumBy (compare `on` fst) $ map (\(c1, c2) -> overlapWithEqAssumption c1 c2 cs1 cs2) allCombinations
  where
    allCombinations = liftM2 (,) cs1 cs2

overlapWithEqAssumption :: Coord -> Coord -> [Coord] -> [Coord] -> (Int, ScannerState)
overlapWithEqAssumption c1 c2 cs1 cs2 = (length $ intersect cs1 transposed, ScannerState transposed diff')
  where
    diff' = diff c1 c2
    transposed = map (translateC diff') cs2

allOrientations :: Coord -> [Coord]
allOrientations (x, y, z) = [(x,y,z),(-y,x,z),(-x,-y,z),(y,-x,z),(-z,y,x),(-y,-z,x),(z,-y,x),(y,z,x),(-x,y,-z),(-y,-x,-z),(x,-y,-z),(y,x,-z),(z,y,-x),(-y,z,-x),(-z,-y,-x),(y,-z,-x),(x,z,-y),(-z,x,-y),(-x,-z,-y),(z,-x,-y),(-x,z,y),(-z,-x,y),(x,-z,y),(z,x,y)]

translateC :: Coord -> Coord -> Coord
translateC (dx, dy, dz) (x, y, z) = (x + dx, y + dy, z + dz)

diff :: Coord -> Coord -> Coord
diff (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)

readScanners :: [BS.ByteString] -> [ScannerData]
readScanners = map readScanner . splitOn [""]

readScanner :: [BS.ByteString] -> ScannerData
readScanner = ScannerData . (map readCoord . tail)

readCoord :: BS.ByteString -> Coord
readCoord bs = (is !! 0, is !! 1, is !! 2)
  where
    is = (map (fst . fromJust . BC.readInt) . BC.split ',') bs
