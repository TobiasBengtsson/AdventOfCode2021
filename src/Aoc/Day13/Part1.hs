module Aoc.Day13.Part1 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Set as Set
import Data.List (break)
import Data.Maybe (fromJust)

solve :: [BS.ByteString] -> String
solve s = show $ Set.size $ fold (head folds) (Set.fromList dots) 
  where
    parts = break (== "") s
    dots  = readDots $ fst parts
    folds = readFolds $ drop 1 $ snd parts

fold :: Fold -> Set.Set (Int, Int) -> Set.Set (Int, Int)
fold fld = Set.map fold'
  where
    fold' (x,y) = case fld of
      X x' -> if x > x' then (x' - (x - x'), y) else (x, y)
      Y y' -> if y > y' then (x, y' - (y - y')) else (x, y)

readDot :: BS.ByteString -> (Int, Int)
readDot s = (head is, is !! 1)
  where
    is = map (fst . fromJust . BC.readInt) $ BC.split ',' s

readDots = map readDot

data Fold = X Int | Y Int

readFold :: BS.ByteString -> Fold
readFold s = cons val
  where
    spl  = BC.split '=' s
    spl' = BC.split ' ' (head spl)
    cons = if last spl' == "x" then X else Y
    val  = (fst . fromJust . BC.readInt) (last spl)

readFolds = map readFold
