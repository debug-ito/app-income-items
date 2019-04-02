-- |
-- Module: App.Income.Items.Convert
-- Description: Convert Monthly report to Zaim entries
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module App.Income.Items.Convert
  ( convert,
    Config(..)
  ) where

import App.Income.Items.Monthly (Report(..))
import App.Income.Items.Zaim (Entry(..))

data Config = Config

convert :: Config -> Report -> [Entry]
convert = undefined
