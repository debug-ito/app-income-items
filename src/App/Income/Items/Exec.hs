-- |
-- Module: App.Income.Items.Exec
-- Description: 
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module App.Income.Items.Exec
  ( mainMonthlyReport
  ) where

import Control.Monad (forM_)
import qualified Data.Text.IO as TIO
import Data.Yaml (decodeFileThrow)
import System.Environment (getArgs)

import App.Income.Items.Monthly (readReport, Report(..), Item(..))
import App.Income.Items.Convert (convert)
import App.Income.Items.Zaim (toCSVString)

mainMonthlyReport :: IO ()
mainMonthlyReport = do
  (config_yaml : filename : _) <- getArgs
  config <- decodeFileThrow config_yaml
  rep <- readReport filename
  putStrLn $ toCSVString $ convert config rep
