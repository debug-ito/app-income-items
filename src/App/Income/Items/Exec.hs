-- |
-- Module: App.Income.Items.Exec
-- Description: 
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module App.Income.Items.Exec
  ( mainMonthlyReport
  ) where

import App.Income.Items.Monthly (readReport, Report(..), Item(..))
import Control.Monad (forM_)
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

mainMonthlyReport :: IO ()
mainMonthlyReport = do
  (filename : _) <- getArgs
  rep <- readReport filename
  forM_ (reportItems rep) $ \item -> do
    TIO.putStr $ itemName item
    putStr " : "
    putStrLn $ show $ itemAmount item
