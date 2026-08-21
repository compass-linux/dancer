module Main where

import System.Environment (getArgs)
import Dancer.CLI (runCLI)

main :: IO ()
main = do
  args <- getArgs
  runCLI args
