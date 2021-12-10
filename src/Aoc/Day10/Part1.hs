module Aoc.Day10.Part1 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Either
import qualified Data.Map as Map
import Data.Maybe

solve :: [BS.ByteString] -> String
solve = show . sum . map score . lefts . map ((`parse` []) . BC.unpack)

safeHead [] = Nothing
safeHead xs = Just (head xs)

data ParseError = Corrupt Char | Incomplete

score Incomplete = 0
score (Corrupt x) = fromJust $ Map.lookup x (Map.fromList $ zip [')', '}', ']', '>'] [3, 1197, 57, 25137])

parse :: String -> String -> Either ParseError ()
parse [] [] = Right ()
parse [] _  = Left Incomplete
parse (x:xs) state
  | x `elem` ['(', '{', '[', '<'] = parse xs (x:state)
  | x `elem` [')', '}', ']', '>'] =
    case safeHead state of
      Just '(' -> if x /= ')' then Left (Corrupt x) else (parse xs (drop 1 state))
      Just '{' -> if x /= '}' then Left (Corrupt x) else (parse xs (drop 1 state))
      Just '[' -> if x /= ']' then Left (Corrupt x) else (parse xs (drop 1 state))
      Just '<' -> if x /= '>' then Left (Corrupt x) else (parse xs (drop 1 state))
      Nothing  -> Left Incomplete
      _        -> error "Input error"
