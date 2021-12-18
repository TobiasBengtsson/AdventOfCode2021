module Aoc.Day18.Part2 where

import qualified Data.ByteString as BS
import Control.Monad

import Aoc.Day18.Part1


solve :: [BS.ByteString] -> String
solve bs = show $ maximum $ map (\(f1, f2) -> max (mag . step $ Pair f1 f2) (mag . step $ Pair f2 f1)) $ filter (uncurry (/=)) $ forest × forest
  where
    forest = readPairs bs

a × b = liftM2 (,) a b
