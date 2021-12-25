module Aoc.Solver where

import qualified Data.ByteString as B

import qualified Aoc.Day1.Part1
import qualified Aoc.Day1.Part2
import qualified Aoc.Day2.Part1
import qualified Aoc.Day2.Part2
import qualified Aoc.Day3.Part1
import qualified Aoc.Day3.Part2
import qualified Aoc.Day4.Part1
import qualified Aoc.Day4.Part2
import qualified Aoc.Day5.Part1
import qualified Aoc.Day5.Part2
import qualified Aoc.Day6.Part1
import qualified Aoc.Day6.Part2
import qualified Aoc.Day7.Part1
import qualified Aoc.Day7.Part2
import qualified Aoc.Day8.Part1
import qualified Aoc.Day8.Part2
import qualified Aoc.Day9.Part1
import qualified Aoc.Day9.Part2
import qualified Aoc.Day10.Part1
import qualified Aoc.Day10.Part2
import qualified Aoc.Day11.Part1
import qualified Aoc.Day11.Part2
import qualified Aoc.Day12.Part1
import qualified Aoc.Day12.Part2
import qualified Aoc.Day13.Part1
import qualified Aoc.Day13.Part2
import qualified Aoc.Day14.Part1
import qualified Aoc.Day14.Part2
import qualified Aoc.Day15.Part1
import qualified Aoc.Day15.Part2
import qualified Aoc.Day16.Part1
import qualified Aoc.Day16.Part2
import qualified Aoc.Day17.Part1
import qualified Aoc.Day17.Part2
import qualified Aoc.Day18.Part1
import qualified Aoc.Day18.Part2
import qualified Aoc.Day19.Part1
import qualified Aoc.Day19.Part2
import qualified Aoc.Day20.Part1
import qualified Aoc.Day20.Part2
import qualified Aoc.Day21.Part1
import qualified Aoc.Day21.Part2
import qualified Aoc.Day22.Part1
import qualified Aoc.Day22.Part2
import qualified Aoc.Day23.Part1
import qualified Aoc.Day23.Part2
import qualified Aoc.Day24.Part1
import qualified Aoc.Day24.Part2
import qualified Aoc.Day25.Part1
import qualified Aoc.Day25.Part2

getSolver :: String -> String -> [B.ByteString] -> String
getSolver "1" "1" = Aoc.Day1.Part1.solve
getSolver "1" "2" = Aoc.Day1.Part2.solve
getSolver "2" "1" = Aoc.Day2.Part1.solve
getSolver "2" "2" = Aoc.Day2.Part2.solve
getSolver "3" "1" = Aoc.Day3.Part1.solve
getSolver "3" "2" = Aoc.Day3.Part2.solve
getSolver "4" "1" = Aoc.Day4.Part1.solve
getSolver "4" "2" = Aoc.Day4.Part2.solve
getSolver "5" "1" = Aoc.Day5.Part1.solve
getSolver "5" "2" = Aoc.Day5.Part2.solve
getSolver "6" "1" = Aoc.Day6.Part1.solve
getSolver "6" "2" = Aoc.Day6.Part2.solve
getSolver "7" "1" = Aoc.Day7.Part1.solve
getSolver "7" "2" = Aoc.Day7.Part2.solve
getSolver "8" "1" = Aoc.Day8.Part1.solve
getSolver "8" "2" = Aoc.Day8.Part2.solve
getSolver "9" "1" = Aoc.Day9.Part1.solve
getSolver "9" "2" = Aoc.Day9.Part2.solve
getSolver "10" "1" = Aoc.Day10.Part1.solve
getSolver "10" "2" = Aoc.Day10.Part2.solve
getSolver "11" "1" = Aoc.Day11.Part1.solve
getSolver "11" "2" = Aoc.Day11.Part2.solve
getSolver "12" "1" = Aoc.Day12.Part1.solve
getSolver "12" "2" = Aoc.Day12.Part2.solve
getSolver "13" "1" = Aoc.Day13.Part1.solve
getSolver "13" "2" = Aoc.Day13.Part2.solve
getSolver "14" "1" = Aoc.Day14.Part1.solve
getSolver "14" "2" = Aoc.Day14.Part2.solve
getSolver "15" "1" = Aoc.Day15.Part1.solve
getSolver "15" "2" = Aoc.Day15.Part2.solve
getSolver "16" "1" = Aoc.Day16.Part1.solve
getSolver "16" "2" = Aoc.Day16.Part2.solve
getSolver "17" "1" = Aoc.Day17.Part1.solve
getSolver "17" "2" = Aoc.Day17.Part2.solve
getSolver "18" "1" = Aoc.Day18.Part1.solve
getSolver "18" "2" = Aoc.Day18.Part2.solve
getSolver "19" "1" = Aoc.Day19.Part1.solve
getSolver "19" "2" = Aoc.Day19.Part2.solve
getSolver "20" "1" = Aoc.Day20.Part1.solve
getSolver "20" "2" = Aoc.Day20.Part2.solve
getSolver "21" "1" = Aoc.Day21.Part1.solve
getSolver "21" "2" = Aoc.Day21.Part2.solve
getSolver "22" "1" = Aoc.Day22.Part1.solve
getSolver "22" "2" = Aoc.Day22.Part2.solve
getSolver "23" "1" = Aoc.Day23.Part1.solve
getSolver "23" "2" = Aoc.Day23.Part2.solve
getSolver "24" "1" = Aoc.Day24.Part1.solve
getSolver "24" "2" = Aoc.Day24.Part2.solve
getSolver "25" "1" = Aoc.Day25.Part1.solve
getSolver "25" "2" = Aoc.Day25.Part2.solve
getSolver _ _ = error "Could not parse arguments correctly"
