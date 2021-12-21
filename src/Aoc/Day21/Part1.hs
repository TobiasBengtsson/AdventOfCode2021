module Aoc.Day21.Part1 where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Char as Char
import Control.Monad.State

data GameState = GameState
  { p1Score  :: Int
  , p2Score  :: Int
  , p1Pos    :: Int
  , p2Pos    :: Int
  , nextDies :: [Int]
  , throwCount :: Int }

solve :: [BC.ByteString] -> String
solve bs = show $ evalState runGame $ GameState { p1Score = 0, p2Score = 0, p1Pos = p1Start, p2Pos = p2Start, nextDies = deterministicDie, throwCount = 0 }
  where
    p1Start = readStart $ head bs
    p2Start = readStart $ bs !! 1

runGame :: State GameState Int
runGame = do
  s <- get
  let p1Step = sum $ take 3 (nextDies s)
  let p1NewPos = ((p1Pos s + p1Step - 1) `mod` 10) + 1
  let p1NewScore = p1Score s + p1NewPos
  if p1NewScore >= 1000 then return (p2Score s * (throwCount s + 3)) else do
    let p2Step = sum $ take 3 $ drop 3 $ nextDies s
    let p2NewPos = ((p2Pos s + p2Step - 1) `mod` 10) + 1
    let p2NewScore = p2Score s + p2NewPos
    if p2NewScore >= 1000 then return (p1NewScore * (throwCount s + 6)) else do
      put s { p1Score = p1NewScore, p2Score = p2NewScore, p1Pos = p1NewPos, p2Pos = p2NewPos, nextDies = drop 6 $ nextDies s, throwCount = throwCount s + 6 }
      runGame

deterministicDie :: [Int]
deterministicDie = cycle [1..100]

readStart :: BC.ByteString -> Int
readStart = Char.digitToInt . BC.last
