module Aoc.Day2.Part2 where

import qualified Data.ByteString as BS
import qualified Aoc.Day2.Part1 as P1

data AimedPosition = AimedPosition {
    position :: P1.Position
  , aim :: Int
}

nullAimedPosition :: AimedPosition
nullAimedPosition = AimedPosition P1.nullPosition 0

solve :: [BS.ByteString] -> String
solve = show . P1.answer . position . (foldl execute nullAimedPosition) . fmap P1.parse

execute :: AimedPosition -> P1.Command -> AimedPosition
execute (AimedPosition (P1.Position h d) a) (P1.Forward i) =
    AimedPosition (P1.Position (h + i) (d + a * i)) a
execute (AimedPosition p a) (P1.Down i) = AimedPosition p (a + i)
execute (AimedPosition p a) (P1.Up i) = AimedPosition p (a - i)
