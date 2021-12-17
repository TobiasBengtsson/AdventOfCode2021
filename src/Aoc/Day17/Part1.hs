module Aoc.Day17.Part1 where
import Aoc.Day17.Part2 (minY)

solve :: a -> String
solve _ = show $ vy0 * (vy0 + 1) `div` 2
  where
    minY = -68
    vy0  = -minY - 1

-- First observation: it is possible to reach vx=0 over the target area (solve n(n+1)/2 witin
-- [minX, maxX] for integer solutions) which means the only limiting factor is in the y dimension.
-- Because of symmetry, if the parable starts with a positive vy0 it will always pass x=0 on
-- its way back.
-- We want the downward velocity to be as large as possible just before hitting the target area,
-- since this means we have maximized the height of the parable.
-- Hence, the final step should be from x=0 to x=minY giving the final vy=0-minY.
-- Again because of symmetry this gives a starting velocity of vy0 = -minY - 1.
-- The maximum height is the sum vy0 + (vy0 - 1) .. 0 which is vy0(vy0 + 1)/2.
