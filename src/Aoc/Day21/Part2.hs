module Aoc.Day21.Part2 where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Char as Char
import qualified Data.Map as Map
import Control.Monad.State
import Debug.Trace

data GameState = GameState
  { p1Score  :: Int
  , p2Score  :: Int
  , p1Pos    :: Int
  , p2Pos    :: Int
  , turn     :: Int }
  deriving (Eq, Ord)

solve :: [BC.ByteString] -> String
solve bs = show $ tplMax $ evalState (runGame initState) Map.empty
  where
    p1Start = readStart $ head bs
    p2Start = readStart $ bs !! 1
    initState = GameState
      { p1Score = 0
      , p2Score = 0
      , p1Pos = p1Start
      , p2Pos = p2Start
      , turn = 1 }

runGame :: GameState -> State (Map.Map GameState (Int, Int)) (Int, Int)
runGame s
  | p1Score s >= 21 = return (1,0)
  | p2Score s >= 21 = return (0,1)
  | otherwise = do
      memo <- get
      case Map.lookup s memo of
        Just x  -> return x
        Nothing -> do
          let quantumStates = mapM (runGame . (`move` s)) combinations
          thisStateScore <- foldl1 tplAdd <$> quantumStates
          modify $ Map.insert s thisStateScore
          return thisStateScore

move :: Int -> GameState -> GameState
move steps s
  | turn s == 1 = let p = newPos (p1Pos s) in
                    s { p1Score = p1Score s + p, p1Pos = p, turn = 2 }
  | otherwise   = let p = newPos (p2Pos s) in
                    s { p2Score = p2Score s + p, p2Pos = p, turn = 1 }
  where
    newPos pos = (pos + steps - 1) `mod` 10 + 1

combinations :: [Int]
combinations = [x + y + z | x <- [1..3], y <- [1..3], z <- [1..3]]

readStart :: BC.ByteString -> Int
readStart = Char.digitToInt . BC.last

tplMax :: (Int, Int) -> Int
tplMax (a,b) = max a b

tplAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)
tplAdd (a,b) (a',b') = (a+a',b+b')
