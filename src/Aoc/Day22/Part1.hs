module Aoc.Day22.Part1 where

import qualified Data.ByteString.Char8 as BC
import qualified Data.MultiSet as MS
import Data.Maybe


newtype Span = Span (Int, Int) deriving (Eq, Ord)

mkSpan a b = Span (a, b)

cardinalitySpan :: Span -> Int
cardinalitySpan (Span (a, b)) | b < a = 0
cardinalitySpan (Span (a, b)) = b - a + 1

intersectSpan :: Span -> Span -> Span
intersectSpan (Span (a1, a2)) (Span (b1, b2)) = Span (max a1 b1, min a2 b2)


data Cuboid = Cuboid
  { spanX :: Span
  , spanY :: Span
  , spanZ :: Span
  } deriving (Eq, Ord)

cardinalityCuboid :: Cuboid -> Int
cardinalityCuboid (Cuboid x y z) = product $ map cardinalitySpan [x, y, z]

intersectCuboid :: Cuboid -> Cuboid -> Cuboid
intersectCuboid (Cuboid x1 y1 z1) (Cuboid x2 y2 z2) =
  Cuboid (intersectSpan x1 x2) (intersectSpan y1 y2) (intersectSpan z1 z2)


data EngineAction = On | Off
data CuboidAction = CuboidAction EngineAction Cuboid

data EngineState = EngineState
  { add :: MS.MultiSet Cuboid
  , subtract :: MS.MultiSet Cuboid
  }

nullEngineState :: EngineState
nullEngineState = EngineState MS.empty MS.empty

cardinalityEngine :: EngineState -> Int
cardinalityEngine (EngineState adds subtracts) =
  sum (MS.map cardinalityCuboid adds) - sum (MS.map cardinalityCuboid subtracts)

gcEngine :: EngineState -> EngineState
gcEngine (EngineState adds subtracts) =
  EngineState (adds MS.\\ equals) (subtracts MS.\\ equals)
  where
    equals = MS.intersection adds subtracts

applyAction :: CuboidAction -> EngineState -> EngineState
applyAction (CuboidAction action c) (EngineState adds subtracts) =
  case action of
    On -> EngineState (MS.insert c $ MS.union subIntersects adds) (MS.union addIntersects subtracts)
    Off -> EngineState (MS.union subIntersects adds) (MS.union addIntersects subtracts)
  where
    addIntersects = MS.map (intersectCuboid c) adds
    subIntersects = MS.map (intersectCuboid c) subtracts


solve :: [BC.ByteString] -> String
solve = solve' . readPart1

solve' :: [CuboidAction] -> String
solve' = show . cardinalityEngine . foldl (\es cs -> gcEngine $ applyAction cs es) nullEngineState


readPart1 :: [BC.ByteString] -> [CuboidAction]
readPart1 = map readLine . take 20

readLine :: BC.ByteString -> CuboidAction
readLine bss = CuboidAction state $ Cuboid (spans !! 0) (spans !! 1) (spans !! 2)
  where
    spl = BC.split ' ' bss
    state = case head spl of
      "on"  -> On
      "off" -> Off
      _     -> error "Unknown state"
    spans = map (\x -> readSpan (BC.split '=' x !! 1)) $ BC.split ',' $ spl !! 1

readSpan :: BC.ByteString -> Span
readSpan bs = mkSpan first second
  where
    (first, rest)  = fromJust $ BC.readInt bs
    second         = fst $ fromJust $ BC.readInt $ BC.dropWhile (== '.') rest
