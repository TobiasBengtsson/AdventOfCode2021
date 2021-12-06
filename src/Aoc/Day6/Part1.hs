module Aoc.Day6.Part1 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Maybe as Maybe
import qualified Data.MultiSet as MS

readDays :: BS.ByteString -> MS.MultiSet Int
readDays = MS.fromList . fmap toInt . BC.split ','

solve :: [BS.ByteString] -> String
solve = show . MS.size . (!! 80) . iterate iter . readDays . head

iter :: MS.MultiSet Int -> MS.MultiSet Int
iter = MS.foldOccur f MS.empty
  where
    f 0 occ = MS.insertMany 8 occ . MS.insertMany 6 occ
    f x occ = MS.insertMany (x-1) occ

toInt :: BS.ByteString -> Int
toInt = fst . Maybe.fromJust . BC.readInt
