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

solve' n bss = show $ last occurs - head occurs
  where
    res = foldr (const $ step (rules bss)) (initial bss) $ replicate n 0
    occurs = sort $ map (`div` 2) $ map snd (MS.toOccurList $ MS.insert headInit $ MS.insert lastInit $ MS.concatMap (\(l,r) -> [l,r]) res)
    headInit = BC.head $ head bss
    lastInit = BC.last $ head bss

initial :: [BS.ByteString] -> MS.MultiSet (Char, Char)
initial = readInitial . BC.unpack . head

rules = readRules . drop 2

step :: Rules -> MS.MultiSet (Char, Char) -> MS.MultiSet (Char, Char)
step pairs = MS.concatMap (`stepPair` pairs)

stepPair :: (Char, Char) -> Rules -> [(Char, Char)]
stepPair p@(l,r) rules = [(l,m),(m,r)]
  where
    m = fromJust $ Map.lookup p rules

readInitial :: String -> MS.MultiSet (Char, Char)
readInitial = MS.fromList . (zip <*> drop 1)

readRules :: [BS.ByteString] -> Rules
readRules = Map.unions . map readRule

readRule :: BS.ByteString -> Rules
readRule bs = Map.singleton (BC.head bs, BC.head $ BC.tail bs) (BC.last bs)
