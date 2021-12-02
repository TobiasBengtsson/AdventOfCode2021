module Aoc.Day2.Part2 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Maybe

import qualified Aoc.Day2.Part1 as Part1

data AimedPosition = AimedPosition {
    position :: Part1.Position
  , aim :: Int
}

nullAimedPosition = AimedPosition (Part1.Position 0 0) 0

solve :: [BS.ByteString] -> String
solve = show . Part1.answer . position . (foldl execute nullAimedPosition) . fmap Part1.parse

execute :: AimedPosition -> Part1.Command -> AimedPosition
execute (AimedPosition (Part1.Position h d) a) (Part1.Forward i) =
    AimedPosition (Part1.Position (h + i) (d + a * i)) a
execute (AimedPosition p a) (Part1.Down i) = AimedPosition p (a + i)
execute (AimedPosition p a) (Part1.Up i) = AimedPosition p (a - i)
