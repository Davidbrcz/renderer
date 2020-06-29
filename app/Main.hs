module Main where

import Parse
import System.IO
import System.Environment


main :: IO ()
main = do
  (file:_) <- getArgs
  content <- readFile file
  case parseFile content of
    Right g -> print g
    Left err -> print err
