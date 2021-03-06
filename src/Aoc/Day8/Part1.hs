module Aoc.Day8.Part1 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Control.Arrow
import Data.Maybe (fromJust)
import Data.List (sort, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.MultiSet as MS

solve :: [BS.ByteString] -> String
solve = show . length . filter (`elem` ['4','1','7','8']) . concatMap solveRow

solveRow :: BS.ByteString -> String
solveRow = readRow >>> uncurry ((MS.fromList . concat >>= (\m o -> Map.fromList $ (`zip` ['1','7','4','2','5','3','6','0','9','8']) (map sort $ sortOn (sum . map (`MS.occur` m)) o))) >>> (\m -> map (fromJust . flip Map.lookup m . sort)))

readRow :: BS.ByteString -> ([String], [String])
readRow = (\x -> (x !! 0, x !! 1)) . map (map BC.unpack . BC.split ' ' . BC.strip) . BC.split '|'
