module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.CPUTime ( getCPUTime )
import System.Environment ( getArgs )
import System.Exit ( exitFailure )
import System.IO ( stderr, hPutStrLn )
import Text.Printf ( printf )

import Aoc.Solver ( getSolver )

main :: IO ()
main = do
    args <- getArgs
    case args of
        [day, part] -> solve day part
        _ -> do
            hPutStrLn stderr "Please provide exactly 2 args: day (1-25) and part (1-2)."
            exitFailure

solve :: String -> String -> IO ()
solve day part = do
    start <- getCPUTime
    file <- B.readFile $ getFileName day
    putStrLn $ getSolver day part $ BC.lines file
    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10^9)
    printf "(total time %0.3f ms)\n" (diff :: Double)

getFileName :: String -> String
getFileName day = concat ["./input/day", day, ".txt"]
