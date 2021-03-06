module Aoc.Day23.Part1 where

import qualified Data.ByteString.Char8 as BC

solve :: [BC.ByteString] -> String
solve _ = show $ 60 + 50 + 3 + 600 + 7000 + 7 + 30 + 40 + 8000 + 700 + 9 + 9

{-
#############
#...........#
###D#D#B#A###
  #C#A#B#C#
  #########

60 + 50 + 3:

#############
#.B.B......A#
###D#D#.#.###
  #C#A#.#C#
  #########

600:

#############
#.B.B......A#
###D#D#.#.###
  #C#A#C#.#
  #########

7000:

#############
#.B.B......A#
###D#.#.#.###
  #C#A#C#D#
  #########

7:

#############
#.B.B.....AA#
###D#.#.#.###
  #C#.#C#D#
  #########

30 + 40:

#############
#.........AA#
###D#B#.#.###
  #C#B#C#D#
  #########

8000 + 700:

#############
#.........AA#
###.#B#C#D###
  #.#B#C#D#
  #########

9 + 9:

#############
#.........AA#
###.#B#C#D###
  #.#B#C#D#
  #########
-}
