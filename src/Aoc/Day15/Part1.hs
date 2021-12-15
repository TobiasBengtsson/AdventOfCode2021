module Aoc.Day15.Part1 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Char
import Data.List
import qualified Data.Set as Set
import Debug.Trace
import Control.Arrow
import Control.Monad.State

type Grid = Map.Map (Int,Int) Int
type PartialGrid = Grid

solve :: [BS.ByteString] -> String
solve = show . runSearch . readGrid

runSearch grid = evalState search $ SearchState (0,0) (maximum $ Map.keys grid) grid (Map.singleton (0,0) 0) (Map.keysSet grid)

data SearchState = SearchState
  { current   :: (Int, Int)
  , target    :: (Int, Int)
  , grid      :: Grid
  , frontier  :: PartialGrid
  , unvisited :: Set.Set (Int, Int) }

search :: State SearchState Int
search = do
  s <- get
  let curr = current s
  let currRisk = fromJust $ Map.lookup curr $ frontier s
  if curr == target s then return currRisk else do
    let neigh = neighbours curr
    let newFrontier = foldl' (\mr p -> updateFrontier currRisk p (unvisited s) (grid s) mr) (frontier s) neigh
    let newFrontier' = Map.delete curr newFrontier
    let newUnvisited = Set.delete curr (unvisited s)
    let next = head $ filter (`Set.member` newUnvisited) $ map fst $ sortOn snd $ Map.toList newFrontier'
    put $ s { current = next, frontier = newFrontier', unvisited = newUnvisited }
    search

updateFrontier currRisk p unvisited grid frontier
  | Map.notMember p grid = frontier
  | Set.notMember p unvisited = frontier
  | Map.notMember p frontier = Map.insert p (currRisk + (fromJust $ Map.lookup p grid)) frontier
  | otherwise = Map.insert p (min (fromJust $ Map.lookup p frontier) (currRisk + fromJust (Map.lookup p grid))) frontier

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x,y) = [(x+1,y),(x,y+1),(x-1,y),(x,y-1)]

readGrid :: [BS.ByteString] -> Grid
readGrid = foldl' (curry $ Map.union *** uncurry readRow >>> uncurry ($)) Map.empty . zip [0..]

readRow :: Int -> BS.ByteString -> PartialGrid
readRow i = Map.fromList . zip [(i,x) | x <- [0..]] . map digitToInt . BC.unpack
