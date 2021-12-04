module Aoc.Day4.Part2 where

import qualified Data.ByteString as BS
import Data.Maybe
import Data.List

import qualified Aoc.Day4.Part1 as P1

solve :: [BS.ByteString] -> String
solve (l:ls) = show $ lastCalledNumber * (P1.boardScore $ fst worstWinningPlay)
  where
    drawingList = P1.readDrawingList l
    boards = P1.readBoards ls
    winAfterMoves = maximum $ fmap snd boardMoves
    lastCalledNumber = drawingList !! (winAfterMoves - 1)
    boardMoves = fmap (\bm -> (fst bm, fromJust $ snd bm)) $ filter (\bm -> isJust $ snd bm) $ fmap (P1.playToWin 1 drawingList) boards
    worstWinningPlay = last $ sortOn snd boardMoves

