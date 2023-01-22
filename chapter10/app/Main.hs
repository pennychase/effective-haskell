module Main where

import System.Environment (getArgs)
import DirectoryTraversal

main :: IO ()
main = getArgs >>= directorySummaryWithMetrics . head
