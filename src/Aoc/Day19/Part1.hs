module Aoc.Day19.Part1 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Control.Monad (liftM2)
import Data.Function
import Data.Maybe
import Data.List.Split (splitOn)
import Data.List (intersect, maximumBy, nub, transpose)
import qualified Data.Set as Set

type Coord = (Int, Int, Int)
data ScannerData = ScannerData
  { sid :: Int
  , readings :: [Coord]
  , location :: Maybe Coord }

solve :: [BS.ByteString] -> String
solve bs = show $ beaconCount $ solve' initScanners Set.empty
  where
    scanners = readScanners bs
    initScanners = (head scanners) { location = Just (0,0,0) } : tail scanners

beaconCount :: [ScannerData] -> Int
beaconCount = length . nub . concatMap readings

solve' :: [ScannerData] -> Set.Set Int -> [ScannerData]
solve' sds alreadyPivoted
  | length solved == length sds = sds
  | otherwise = solve' (map (solveWithPivot pivot) sds) (Set.insert (sid pivot) alreadyPivoted)
  where
    solved = filter (isJust . location) sds
    pivot = head $ filter (\sd -> Set.notMember (sid sd) alreadyPivoted) solved

solveWithPivot :: ScannerData -> ScannerData -> ScannerData
solveWithPivot pivot sd
  | isJust (location sd)   = sd
  | length (fst3 ol) >= 12 = sd { readings = last3 ol, location = Just $ mdl3 ol }
  | otherwise              = sd
  where
    ol = overlaps (readings pivot) (readings sd)

fst3 (a,b,c) = a
mdl3 (a,b,c) = b
last3 (a,b,c) = c

overlaps :: [Coord] -> [Coord] -> ([Coord], Coord, [Coord])
overlaps cs1 cs2 = maximumBy (compare `on` (length . fst3)) $ map (overlapsOrientation cs1) orientations
  where
    orientations = transpose $ map allOrientations cs2

overlapsOrientation :: [Coord] -> [Coord] -> ([Coord], Coord, [Coord])
overlapsOrientation cs1 cs2 = maximumBy (compare `on` (length . fst3)) $ map (\(c1, c2) -> overlapWithEqAssumption c1 c2 cs1 cs2) allCombinations
  where
    allCombinations = liftM2 (,) cs1 cs2

overlapWithEqAssumption :: Coord -> Coord -> [Coord] -> [Coord] -> ([Coord], Coord, [Coord])
overlapWithEqAssumption c1 c2 cs1 cs2 = (intersect cs1 transposed, diff', transposed)
  where
    diff' = diff c1 c2
    transposed = map (transposeC diff') cs2

allOrientations :: Coord -> [Coord]
allOrientations (x, y, z) = [(x,y,z),(-y,x,z),(-x,-y,z),(y,-x,z),(-z,y,x),(-y,-z,x),(z,-y,x),(y,z,x),(-x,y,-z),(-y,-x,-z),(x,-y,-z),(y,x,-z),(z,y,-x),(-y,z,-x),(-z,-y,-x),(y,-z,-x),(x,z,-y),(-z,x,-y),(-x,-z,-y),(z,-x,-y),(-x,z,y),(-z,-x,y),(x,-z,y),(z,x,y)]

transposeC :: Coord -> Coord -> Coord
transposeC (dx, dy, dz) (x, y, z) = (x + dx, y + dy, z + dz)

diff :: Coord -> Coord -> Coord
diff (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)

readScanners :: [BS.ByteString] -> [ScannerData]
readScanners = zipWith (\i x -> x i) [0..] . map readScanner . splitOn [""]

readScanner :: [BS.ByteString] -> Int -> ScannerData
readScanner bs id = ScannerData id ((map readCoord . tail) bs) Nothing

readCoord :: BS.ByteString -> Coord
readCoord bs = (is !! 0, is !! 1, is !! 2)
  where
    is = (map (fst . fromJust . BC.readInt) . BC.split ',') bs
