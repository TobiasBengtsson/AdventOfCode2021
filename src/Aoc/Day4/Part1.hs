module Aoc.Day4.Part1 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Maybe
import Data.List
import Data.List.Split (splitOn)

type BingoRow = [(Bool, Int)]
data BingoBoard = BingoBoard [BingoRow] deriving Show

bingoRow is = fmap (\x -> (False, x)) is

markNumber :: Int -> BingoBoard -> BingoBoard
markNumber n (BingoBoard rows) = BingoBoard (fmap markRow rows)
  where
    markRow :: BingoRow -> BingoRow 
    markRow row = fmap markCell row
    markCell :: (Bool, Int) -> (Bool, Int)
    markCell cell = case cell of
      (False, x) | x == n -> (True, x)
      otherwise           -> cell

hasWon :: BingoBoard -> Bool
hasWon (BingoBoard rows) = (or $ fmap rowComplete (transpose rows)) || (or $ fmap rowComplete rows)
  where
    rowComplete :: BingoRow -> Bool
    rowComplete cells = and $ fmap fst cells

boardScore :: BingoBoard -> Int
boardScore (BingoBoard rows) = sum $ fmap rowScore rows
  where
    rowScore cells = sum $ fmap cellScore cells
    cellScore :: (Bool, Int) -> Int
    cellScore (mark, n) = if mark then 0 else n

readBoard :: [BS.ByteString] -> BingoBoard
readBoard = BingoBoard . fmap bingoRow . fmap (fmap toInt . BC.words)

readBoards :: [BS.ByteString] -> [BingoBoard]
readBoards = fmap readBoard . splitOn [""]

readDrawingList :: BS.ByteString -> [Int]
readDrawingList = fmap toInt . BC.split ','

solve :: [BS.ByteString] -> String
solve (l:ls) = show $ lastCalledNumber * (boardScore $ fst bestPlay)
  where
    drawingList = readDrawingList l
    boards = readBoards ls
    winAfterMoves = minimum $ fmap snd boardMoves
    lastCalledNumber = drawingList !! (winAfterMoves - 1)
    boardMoves = fmap (\bm -> (fst bm, fromJust $ snd bm)) $ filter (\bm -> isJust $ snd bm) $ fmap (playToWin 1 drawingList) boards
    bestPlay = head $ sortOn snd boardMoves

playToWin :: Int -> [Int] -> BingoBoard -> (BingoBoard, Maybe Int)
playToWin _ [] board = (board, Nothing)
playToWin n (x:xs) board = case hasWon board' of
    True -> (board', Just n)
    False -> playToWin (n + 1) xs board'
  where
    board' = markNumber x board

toInt = fst . fromJust . BC.readInt
    
    
