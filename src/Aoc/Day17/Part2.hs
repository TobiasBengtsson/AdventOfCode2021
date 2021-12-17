module Aoc.Day17.Part2 where

import Control.Monad

solve :: a -> String
solve _ = show $ length $ filter (possibleVXY (0,0)) $ possibleVXs × possibleVYs

a × b = liftM2 (,) a b

possibleVXs = filter (possibleVX 0) [0..maxX]

possibleVX x vx
 | x >= minX && x <= maxX = True
 | x > maxX               = False
 | vx == 0                = False
 | otherwise              = possibleVX (x + vx) (vx - 1)

possibleVYs = filter (possibleVY 0) [minY, (minY + 1)..(-minY)]

possibleVY y vy
 | y >= minY && y <= maxY = True
 | y < minY               = False
 | otherwise              = possibleVY (y + vy) (vy - 1)

possibleVXY (x,y) (vx, vy)
 | x >= minX && x <= maxX
   && y >= minY && y <= maxY = True
 | x > maxX                  = False
 | y < minY                  = False
 | otherwise                 = possibleVXY (x + vx, y + vy) (max (vx - 1) 0, vy - 1)

minX = 269
maxX = 292

maxY = -44
minY = -68
