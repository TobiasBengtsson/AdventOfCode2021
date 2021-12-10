module Aoc.Day10.Part2 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Either (lefts)
import Data.List (sort)
import Data.Maybe (fromMaybe)

import Aoc.Day10.Part1

solve :: [BS.ByteString] -> String
solve = show . middle . filter (/= 0) . map incompleteScore . lefts . map ((`parse` []) . BC.unpack)

middle :: [Int] -> Int
middle xs = sort xs !! (length xs `div` 2)

incompleteScore :: ParseError -> Int
incompleteScore (Incomplete state) = foldl (\acc c -> acc * 5 + incompleteCharScore c) 0 state
incompleteScore _ = 0

incompleteCharScore :: Char -> Int
incompleteCharScore = fromMaybe 0 . flip lookup (zip openings [1, 2, 3, 4])
