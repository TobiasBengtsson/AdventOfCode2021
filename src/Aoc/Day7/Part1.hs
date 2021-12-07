module Aoc.Day7.Part1 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (fromJust)

readCrabs :: BS.ByteString -> [Int]
readCrabs = fmap (fst . fromJust . BC.readInt) . BC.split ','

cost :: Int -> Int -> Int
cost lineX subX = abs (subX - lineX)

costSum :: Int -> [Int] -> Int
costSum lineX = sum . fmap (cost lineX)

solve :: [BS.ByteString] -> String
solve = show . minimum . (\x -> fmap (`costSum` x) [minimum x..maximum x]) . readCrabs . head
