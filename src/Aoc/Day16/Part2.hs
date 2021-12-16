module Aoc.Day16.Part2 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Aoc.Day16.Part1

solve :: [BS.ByteString] -> String
solve = show . eval . parse . readBin . BC.unpack . head

eval (Literal _ v) = v
eval (Op _ op _ ps) = op $ map eval ps
