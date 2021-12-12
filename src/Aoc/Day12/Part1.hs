module Aoc.Day12.Part1 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char
import Data.List

type Edge = (String, String)
type Path = [String]

solve :: [BS.ByteString] -> String
solve = show . length . allPaths "start" "end" . map readRow

allPaths :: String -> String -> [(String, String)] -> [[String]]
allPaths start end edges = allPaths' start end edges []

allPaths' :: String -> String -> [Edge] -> Path -> [Path]
allPaths' start end edges visited
  | start == end = [end:visited]
  | start == "start" && not (null visited) = []
  | isSmallNode start && (start `elem` visited) = []
  | otherwise = concatMap (\s -> allPaths' s end edges (start:visited)) $ adjNodes start edges

adjNodes :: String -> [(String, String)] -> [String]
adjNodes n = nub . map (\e -> if fst e == n then snd e else fst e) . filter (\e -> fst e == n || snd e == n)

isSmallNode :: String -> Bool
isSmallNode = isLower . head
                
readRow :: BS.ByteString -> (String, String)
readRow bs = (head splitString, splitString !! 1)
  where splitString = map BC.unpack $ BC.split '-' bs 

