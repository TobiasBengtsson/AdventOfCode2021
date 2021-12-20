module Aoc.Day20.Part1 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.List (sortOn)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set

type Coord = (Int, Int)

adjCoords :: Coord -> [Coord]
adjCoords (x, y) = sortOn snd $ sortOn fst [(x+x',y+y') | x' <- [-1,0,1], y' <- [-1,0,1]]


data PixelState = Dark | Lit deriving Eq

readPs '#' = Lit
readPs '.' = Dark
readPs _   = undefined

showPs Lit  = '#'
showPs Dark = '.'

toBin :: PixelState -> Int
toBin Lit  = 1
toBin Dark = 0


type Algo = Map.Map Int PixelState

readAlgo :: BS.ByteString -> Map.Map Int PixelState
readAlgo = Map.fromList . zip [0..] . map readPs .  BC.unpack


data Image = Image
  { litCoords :: Set.Set Coord
  , minX      :: Int
  , minY      :: Int
  , maxX      :: Int
  , maxY      :: Int
  , defState  :: PixelState }

readImg :: [BS.ByteString] -> Image
readImg bs = Image
  { litCoords = foldl (\set (y, l) -> Set.union (readImgRow y l) set) Set.empty $ zip [0..] bs
  , minX      = 0
  , maxX      = maximum (map BS.length bs) - 1
  , minY      = 0
  , maxY      = length bs - 1
  , defState  = Dark }

readImgRow :: Int -> BS.ByteString -> Set.Set Coord
readImgRow y = foldl (\set (x, c) -> insertIfLit (x,y) (readPs c) set) Set.empty . zip [0..] . BC.unpack

showImg :: Image -> String
showImg img = unlines $ map (\y -> map (\x -> showPs (psLookup (x,y) img)) [minX img..maxX img]) [minY img..maxY img]

inBoundary :: Coord -> Image -> Bool
inBoundary (x,y) img = x >= minX img && x <= maxX img && y >= minY img && y <= maxY img

psLookup :: Coord -> Image -> PixelState
psLookup coord img
  | not $ inBoundary coord img       = defState img
  | Set.member coord (litCoords img) = Lit
  | otherwise                        = Dark

insertIfLit :: Coord -> PixelState -> Set.Set Coord -> Set.Set Coord
insertIfLit c ps = if ps == Lit then Set.insert c else id

enhancePixel :: Coord -> Algo -> Image -> PixelState
enhancePixel c algo img = res
  where
    bits = map (toBin . (`psLookup` img)) $ adjCoords c
    val = sum $ zipWith (*) (iterate (2*) 1) $ reverse bits
    res = fromJust $ Map.lookup val algo

enhanceImage :: Algo -> Image -> Image
enhanceImage algo img = Image
  { litCoords = foldl (\newCoords c -> insertIfLit c (enhancePixel c algo img) newCoords) Set.empty allCoords
  , minX = newMinX
  , maxX = newMaxX
  , minY = newMinY
  , maxY = newMaxY
  , defState    = if defState img == Lit && snd (fromJust (Map.lookupMax algo)) == Dark then Dark else
                  if defState img == Dark && snd (fromJust (Map.lookupMin algo)) == Lit then Lit else
                     defState img }
  where
    newMinX = minX img - 1
    newMaxX = maxX img + 1
    newMinY = minY img - 1
    newMaxY = maxY img + 1
    allCoords = [(x,y) | x <- [newMinX..newMaxX], y <- [newMinY..newMaxY]]


solve :: [BS.ByteString] -> String
solve = solve' 2

solve' :: Int -> [BS.ByteString] -> String
solve' n bs = show $ Set.size $ litCoords $ iterate (enhanceImage algo) image !! n
  where
    algo = readAlgo $ head bs
    image = readImg $ drop 2 bs
