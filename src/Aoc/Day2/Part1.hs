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

solve :: [BS.ByteString] -> String
solve = show . answer . (foldl execute $ Position 0 0) . fmap parse

answer :: Position -> Int
answer p = horizontal p * depth p

parse :: BS.ByteString -> Command
parse line = case BC.split ' ' line of
    ["forward", i] -> Forward $ toInt i
    ["down", i]    -> Down $ toInt i
    ["up", i]      -> Up $ toInt i
    otherwise      -> error "Could not parse input"
  where
    toInt = fst . fromJust . BC.readInt

execute :: Position -> Command -> Position
execute p (Forward i) = Position { horizontal = horizontal p + i, depth = depth p}
execute p (Down i) = Position { horizontal = horizontal p, depth = depth p + i}
execute p (Up i) = Position { horizontal = horizontal p, depth = depth p - i}
