module Main where

import Anatomy.Evaluator
import System.Environment (getArgs)
import Control.Monad

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \ program -> do
    let result = interpret program
    print result
