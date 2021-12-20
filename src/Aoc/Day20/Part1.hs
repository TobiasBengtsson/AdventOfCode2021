module Aoc.Day20.Part1 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.List (sortOn)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Debug.Trace

type Coord = (Int, Int)
data PixelState = Dark | Lit deriving (Eq, Show)
data Image = Image
  { coords      :: Set.Set Coord
  , boundaryMin :: Coord
  , boundaryMax :: Coord
  , defState    :: PixelState }
  deriving Show

type Algo = Map.Map Int PixelState

cToPixelState '#' = Lit
cToPixelState '.' = Dark
cToPixelState _   = undefined

toBin :: PixelState -> Int
toBin Lit  = 1
toBin Dark = 0

inBoundary :: Coord -> Image -> Bool
inBoundary (x,y) img = x >= (fst $ boundaryMin img)
                    && x <= (fst $ boundaryMax img)
                    && y >= (snd $ boundaryMin img)
                    && y <= (snd $ boundaryMax img)

psLookup :: Coord -> Image -> PixelState
psLookup coord img
  | not $ inBoundary coord img    = defState img
  | Set.member coord (coords img) = Lit
  | otherwise                     = Dark

insertPs :: Coord -> PixelState -> Set.Set Coord -> Set.Set Coord
insertPs c ps = if ps == Lit then Set.insert c else id

enhancePixel :: Coord -> Algo -> Image -> PixelState
enhancePixel c algo img = res
  where
    bits = map (toBin . (`psLookup` img)) $ adjCoords c
    val = sum $ zipWith (*) (iterate (2*) 1) $ reverse bits
    res = fromJust $ Map.lookup val algo

enhanceImage :: Algo -> Image -> Image
enhanceImage algo img = Image
  { coords = foldl (\newCoords c -> insertPs c (enhancePixel c algo img) newCoords) Set.empty allCoords
  , boundaryMin = (minX, minY)
  , boundaryMax = (maxX, maxY)
  , defState    = if defState img == Lit then Dark else Lit }
  where
    minX = (fst $ boundaryMin img) - 1
    maxX = (fst $ boundaryMax img) + 1
    minY = (snd $ boundaryMin img) - 1
    maxY = (snd $ boundaryMax img) + 1
    allCoords = [(x,y) | x <- [minX..maxX], y <- [minY..maxY]]

adjCoords :: Coord -> [Coord]
adjCoords (x, y) = sortOn snd $ sortOn fst [(x+x',y+y') | x' <- [-1,0,1], y' <- [-1,0,1]]

solve :: [BS.ByteString] -> String
solve = solve' 2

solve' :: Int -> [BS.ByteString] -> String
solve' n bs = show $ Set.size $ coords $ iterate (enhanceImage algo) image !! n
  where
    algo = readAlgo $ head bs
    image = readImage $ drop 2 bs

readAlgo :: BS.ByteString -> Map.Map Int PixelState
readAlgo = Map.fromList . zip [0..] . map cToPixelState .  BC.unpack

readImage :: [BS.ByteString] -> Image
readImage bs = Image { coords = cs, boundaryMin = (0,0), boundaryMax = (99,99), defState = Dark }
  where
    cs = foldl (\set (y, l) -> Set.union (readImageRow y l) set) Set.empty $ zip [0..] bs

readImageRow :: Int -> BS.ByteString -> Set.Set Coord
readImageRow y = foldl (\set (x, c) -> if cToPixelState c == Lit then Set.insert (x, y) set else set) Set.empty . zip [0..] . BC.unpack
