module Aoc.Day8.Part1 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (fromJust)
import Data.List (sort, sortOn)
import qualified Data.Map.Strict as Map

solve :: [BS.ByteString] -> String
solve = show . length . filter (`elem` [4,1,7,8]) . concatMap solveRow

solveRow :: BS.ByteString -> [Int]
solveRow s = map display output
  where
    obs = fst $ readRow s
    output = snd $ readRow s
    magic = Map.fromList $ zip (map sort $ sortOn charCountSum $ sortOn length obs) [1,7,4,2,5,3,6,0,9,8]
    charCount :: Char -> Int
    charCount c = elemCount c (concat obs)
    charCountSum = sum . map charCount
    elemCount :: Eq a => a -> [a] -> Int
    elemCount e = length . filter (== e)
    display s = fromJust $ Map.lookup (sort s) magic

readRow :: BS.ByteString -> ([String], [String])
readRow = (\x -> (x !! 0, x !! 1)) . map (map BC.unpack . BC.split ' ' . BC.strip) . BC.split '|'
