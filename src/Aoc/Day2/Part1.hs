module Aoc.Day2.Part1 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Maybe

data Command =
    Forward Int
  | Down Int
  | Up Int
  deriving (Show)

data Position = Position {
    horizontal :: Int
  , depth      :: Int
} deriving (Show)

nullPosition :: Position
nullPosition = Position 0 0

solve :: [BS.ByteString] -> String
solve = show . answer . (foldl execute nullPosition) . fmap parse

answer :: Position -> Int
answer (Position h d) = h * d

parse :: BS.ByteString -> Command
parse line = case BC.split ' ' line of
    ["forward", i] -> Forward $ toInt i
    ["down", i]    -> Down $ toInt i
    ["up", i]      -> Up $ toInt i
    otherwise      -> error "Could not parse input"
  where
    toInt = fst . fromJust . BC.readInt

execute :: Position -> Command -> Position
execute (Position h d) (Forward i) = Position (h + i) d
execute (Position h d) (Down i) = Position h (d + i)
execute (Position h d) (Up i) = Position h (d - i)
