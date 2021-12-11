module Aoc.Day11.Part1 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Control.Arrow ((>>>), Arrow((***)))
import Control.Monad.State.Lazy (MonadState(put, get), State, runState)
import Data.Char (digitToInt)
import qualified Data.Map as Map
import qualified Data.Set as Set

type Grid = Map.Map (Int,Int) Int
type PartialGrid = Grid

solve :: [BS.ByteString] -> String
solve input = show $ fst $ foldr (\i (s,g) -> (fst (step g) + s, snd (step g))) (0, readGrid input) (replicate 100 0)

step :: Grid -> (Int, Grid)
step = runFlashStep . fmap (+1)

runFlashStep :: Grid -> (Int, Grid)
runFlashStep = fmap grid . runState flashStep . GridState 0 Set.empty

data GridState = GridState
  { flashes        :: Int
  , alreadyFlashed :: Set.Set (Int,Int)
  , grid           :: Grid }

flashStep :: State GridState Int
flashStep = do
  s <- get
  let g = grid s
  let af = alreadyFlashed s
  let pending = Map.keys $ Map.filterWithKey (\k v -> Set.notMember k af && v > 9) g
  if null pending then return (flashes s) else do
    let flashedGrid = foldr (`Map.insert` 0) g pending
    let af' = Set.union (alreadyFlashed s) $ Set.fromList pending
    let energizedGrid = foldr (\p g -> foldr (Map.adjust (+1)) g (filter (`Set.notMember` af') (allDirections p))) flashedGrid pending
    put GridState { flashes        = flashes s + length pending
                  , alreadyFlashed = af'
                  , grid           = energizedGrid }
    flashStep

allDirections :: (Int, Int) -> [(Int, Int)]
allDirections (x,y) = [(x+1,y),(x+1,y+1),(x,y+1),(x-1,y+1),(x-1,y),(x-1,y-1),(x,y-1),(x+1,y-1)]

readGrid :: [BS.ByteString] -> Grid
readGrid = foldl (curry $ Map.union *** uncurry readRow >>> uncurry ($)) Map.empty . zip [0..]

readRow :: Int -> BS.ByteString -> PartialGrid
readRow i = Map.fromList . zip [(i,x) | x <- [0..]] . map digitToInt . BC.unpack
