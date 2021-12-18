module Aoc.Day18.Part1 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char

data Pair = Regular Int | Pair Pair Pair deriving Eq

solve :: [BS.ByteString] -> String
solve = show . mag . foldl1 (\p1 p2 -> step $ Pair p1 p2) . readPairs

explodePair :: Int -> Pair -> Maybe (Int, Int, Pair)
explodePair 4 (Pair (Regular i1) (Regular i2)) = Just (i1, i2, Regular 0)
explodePair depth (Pair p1 p2) = case explodePair (depth + 1) p1 of
  Just (i1, i2, p') -> Just (i1, 0, Pair p' (addLeft i2 p2))
  Nothing -> (\(i1, i2, p') -> (0, i2, Pair (addRight i1 p1) p')) <$> explodePair (depth + 1) p2
explodePair _ p = Nothing

explode :: Pair -> Maybe (Int, Int, Pair)
explode = explodePair 0

splitPair :: Pair -> Maybe Pair
splitPair (Regular i)
  | i >= 10   = Just (Pair (Regular (i `div` 2)) (Regular ((i+1) `div` 2)))
  | otherwise = Nothing
splitPair (Pair p1 p2) = case splitPair p1 of
  Just p1' -> Just (Pair p1' p2)
  Nothing  -> Pair p1 <$> splitPair p2

step :: Pair -> Pair
step p = case explode p of
  Just (_, _, p') -> step p'
  Nothing -> maybe p step (splitPair p)

mag :: Pair -> Int
mag (Regular i) = i
mag (Pair p1 p2) = 3 * mag p1 + 2 * mag p2

addLeft :: Int -> Pair -> Pair
addLeft i (Pair p1 p2) = Pair (addLeft i p1) p2
addLeft i1 (Regular i2) = Regular (i1 + i2)

addRight :: Int -> Pair -> Pair
addRight i (Pair p1 p2) = Pair p1 (addRight i p2)
addRight i1 (Regular i2) = Regular (i1 + i2)

readPairs :: [BS.ByteString] -> [Pair]
readPairs = map readPair

readPair :: BS.ByteString -> Pair
readPair bs
  | BC.head bs == '[' = Pair ((readPair . fst) spl) ((readPair . BS.drop 1 . snd) spl)
  | otherwise         = (Regular . digitToInt . BC.head) bs
  where
    spl   = (BS.splitAt =<< findComma 0 0) subBs
    subBs = (BS.tail . BS.init) bs

findComma :: Int -> Int -> BS.ByteString -> Int
findComma depth i bs
  | BC.head bs == ',' && depth == 0 = i
  | BC.head bs == '['               = findComma (depth + 1) (i + 1) (BS.tail bs)
  | BC.head bs == ']'               = findComma (depth - 1) (i + 1) (BS.tail bs)
  | otherwise                       = findComma depth (i + 1) (BS.tail bs)
