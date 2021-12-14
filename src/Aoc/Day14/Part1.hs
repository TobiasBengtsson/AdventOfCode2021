module Aoc.Day14.Part1 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as Map
import Data.List (break, sort, sortOn, foldl')
import Data.Maybe (fromJust)
import qualified Data.MultiSet as MS

type Rules = Map.Map (Char, Char) Char

solve :: [BS.ByteString] -> String
solve = solve' 10

solve' n bss = show ((last occurs - head occurs) `div` 2)
  where
    res = foldl' (\p _ -> step p (rules bss)) (initial bss) (replicate n 0)
    occurs = sort $ map snd (MS.toOccurList $ MS.concatMap (\(l,r) -> [l,r]) res)

initial bss = readInitial $ BC.unpack $ head $ fst $ break (== "") bss

rules bss = readRules (drop 1 $ snd $ break (== "") bss)

step :: MS.MultiSet (Char, Char) -> Rules -> MS.MultiSet (Char, Char)
step pairs rules = MS.concatMap (`stepPair` rules) pairs

stepPair :: (Char, Char) -> Rules -> [(Char, Char)]
stepPair p@(l,r) rules = [(l,m),(m,r)]
  where
    m = fromJust $ Map.lookup p rules

readInitial :: String -> MS.MultiSet (Char, Char)
readInitial s = MS.fromList $ zip s $ drop 1 s

readRules :: [BS.ByteString] -> Rules
readRules = Map.unions . map readRule

readRule :: BS.ByteString -> Rules
readRule bs = Map.singleton (BC.head bs, BC.head $ BC.tail bs) (BC.last bs)
