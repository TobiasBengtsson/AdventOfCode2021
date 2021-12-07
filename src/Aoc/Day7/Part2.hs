module Aoc.Day7.Part2 where

import qualified Aoc.Day7.Part1 as P1
import qualified Data.ByteString as BS

seriesSum :: Int -> Int
seriesSum n = (n * (n + 1)) `div` 2

cost :: Int -> Int -> Int
cost lineX subX = seriesSum (abs (subX - lineX))

costSum :: Int -> [Int] -> Int
costSum lineX = sum . fmap (cost lineX)

solve :: [BS.ByteString] -> String
solve = show . minimum . (\x -> fmap (`costSum` x) [minimum x..maximum x]) . P1.readCrabs . head
