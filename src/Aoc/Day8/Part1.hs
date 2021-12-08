module Aoc.Day8.Part1 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Control.Monad
import Data.Maybe (fromJust)
import Data.List (find, sort)
import qualified Data.MultiSet as MS

solve :: [BS.ByteString] -> String
solve = show .  sum . map solveRow

solveRow :: BS.ByteString -> Int
solveRow s = length $ filter (`elem` [1,4,7,8]) $ map (display . (`mapSignal` charmap)) output
  where
    obs = fst $ readRow s
    output = snd $ readRow s
    charmap = reduceCompleted $ filterCharCounts obs $ foldr filterEasy allMappings obs

readRow :: BS.ByteString -> ([String], [String])
readRow = (\x -> (x !! 0, x !! 1)) . map (map BC.unpack . BC.split ' ' . BC.strip) . BC.split '|'

-- Mappings from actual values to the original ones
allMappings :: [(Char, Char)]
allMappings = liftM2 (,) alphabet alphabet
  where
    alphabet = ['a'..'g']

filterEasy :: String -> [(Char, Char)] -> [(Char, Char)]
filterEasy cs = filter f
  where
    f (key, val)
      | length cs == 2 && key `elem` cs = val `elem` ['c','f']
      | length cs == 4 && key `elem` cs = val `elem` ['b','c','d','f']
      | length cs == 3 && key `elem` cs = val `elem` ['a','c','f']
      | otherwise                       = True

filterCharCounts :: [String] -> [(Char, Char)] -> [(Char, Char)]
filterCharCounts ss map = MS.foldOccur f map $ MS.fromList $ concat ss
  where
    f char occ m
      | occ == 4 = fVals char ['e'] m
      | occ == 6 = fVals char ['b'] m
      | occ == 7 = fVals char ['d','g'] m
      | occ == 8 = fVals char ['a','c'] m
      | occ == 9 = fVals char ['f'] m
      | otherwise = m
    fVals char vals m = filter (\(key, val) -> key /= char || val `elem` vals) m

reduceCompleted :: [(Char, Char)] -> [(Char, Char)]
reduceCompleted m = foldr f m completedKeys
  where
    keys = map fst m
    completedKeys = filter (\(k, v) -> length (filter (== k) keys) == 1) m
    f (k, v) m = filter (\(k',v') -> v /= v' || k == k') m

mapSignal :: String -> [(Char, Char)] -> String
mapSignal s m = map (\x -> snd (fromJust $ find (\y -> fst y == x) m)) s

display :: String -> Int
display s = case sort s of
  "abcefg"  -> 0
  "cf"      -> 1
  "acdeg"   -> 2
  "acdfg"   -> 3
  "bcdf"    -> 4
  "abdfg"   -> 5
  "abdefg"  -> 6
  "acf"     -> 7
  "abcdefg" -> 8
  "abcdfg"  -> 9
  _         -> error "wrong input"
