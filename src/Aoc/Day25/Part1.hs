module Aoc.Day25.Part1 where

import qualified Data.ByteString.Char8 as BC
import Data.List

simulateStep :: [String] -> [String]
simulateStep s = transpose $ map (moveLine 'v') (transpose $ map (moveLine '>') s)

moveLine :: Char -> String -> String
moveLine c s = fstC : (zipWith3 (moveLineStep c) <*> drop 1 <*> drop 2) s ++ [lastC]
  where
    fstC  = moveLineStep c (last s) (head s) (head $ tail s)
    lastC = moveLineStep c (last $ init s) (last s) (head s)

moveLineStep :: Char -> Char -> Char -> Char -> Char
moveLineStep c prevC thisC nextC = case (prevC, thisC, nextC) of
  (c', '.', _  ) | c' == c -> c
  (_ , c' , '.') | c' == c -> '.'
  (_ , c' , _  )           -> c'

solve :: [BC.ByteString] -> String
solve = show . solve' 1 . map BC.unpack

solve' :: Int -> [String] -> Int
solve' n ss = if ss == thisIter then n else nextIter
  where
    thisIter = simulateStep ss
    nextIter = solve' (n + 1) thisIter
