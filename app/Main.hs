module Main (main) where

import App.Income.Items.Monthly (readReport)
import System.Environment (getArgs)

main :: IO ()
main = do
  (filename : _) <- getArgs
  print =<< readReport filename
