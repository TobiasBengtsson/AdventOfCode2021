module Aoc.Day16.Part1 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Maybe
import Data.List
import Data.Char
import Numeric
import Text.Printf
import Debug.Trace

type Version = Int
type LengthTypeId = Int

data Packet = Literal Version Int
            | Op Version ([Int]->Int) LengthTypeId [Packet]

packetLen :: Packet -> Int
packetLen (Literal _ v) = 6 + 5 * truncate (1 + logBase 16.0 (fromIntegral v))
packetLen (Op _ _ 0 ps)   = 22 + sum (map packetLen ps)
packetLen (Op _ _ 1 ps)   = 18 + sum (map packetLen ps)
packetLen _ = error "Invalid case"

versionSum :: Packet -> Int
versionSum (Literal v _) = v
versionSum (Op v _ _ ps)   = v + sum (map versionSum ps)

solve :: [BS.ByteString] -> String
solve = show . versionSum . parse . readBin . BC.unpack . head

parse :: String -> Packet
parse s = case type' s of
  4 -> Literal (version s) (parseLiteral (drop 6 s))
  _ -> case lengthTypeId s of
    0 -> Op (version s) (getOperator s) 0 (parseB (length15 s) (drop 22 s))
    1 -> Op (version s) (getOperator s) 1 (parseN (length11 s) (drop 18 s))
    _ -> error "Invalid lengthTypeId"

getOperator s = case type' s of
  0 -> sum
  1 -> product
  2 -> minimum
  3 -> maximum
  5 -> (\xs -> if xs !! 0 > xs !! 1 then 1 else 0)
  6 -> (\xs -> if xs !! 0 < xs !! 1 then 1 else 0)
  7 -> (\xs -> if xs !! 0 == xs !! 1 then 1 else 0)
  _ -> error "undefined"

parseB :: Int -> String -> [Packet]
parseB 0 _ = []
parseB x _ | x<8 = []
parseB len s = thisPacket : parseB (len - thisPacketLen) (drop thisPacketLen s)
  where
    thisPacket    = parse s
    thisPacketLen = packetLen thisPacket

parseN :: Int -> String -> [Packet]
parseN 0 _ = []
parseN x _ | x<0 = error "Packet count negative"
parseN n s = thisPacket : parseN (n-1) (drop thisPacketLen s)
  where 
    thisPacket    = parse s
    thisPacketLen = packetLen thisPacket

parseLiteral :: String -> Int
parseLiteral = binToInt . getLiteralBytestring

getLiteralBytestring :: String -> String
getLiteralBytestring s | length s < 5 = []
getLiteralBytestring s = if head s == '0' then drop 1 $ take 5 s else drop 1 (take 5 s) ++ getLiteralBytestring (drop 5 s)

version :: String -> Int
version = binToInt . take 3

type' :: String -> Int
type' = binToInt . take 3 . drop 3

lengthTypeId = binToInt . take 1 . drop 6

length11 = binToInt . take 11 . drop 7

length15 = binToInt . take 15 . drop 7

binToInt :: String -> Int
binToInt = foldl' (\acc x -> acc * 2 + digitToInt x) 0

readBin :: String -> String
readBin = concatMap (fromJust . readChar)

readChar :: Char -> Maybe String
readChar c = case readHex [c] of
  (x,_):_ -> Just $ printf "%04b" (x::Int)
  _       -> Nothing
