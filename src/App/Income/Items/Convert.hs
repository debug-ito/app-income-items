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

import Data.Text (Text)

import App.Income.Items.Monthly (Report(..))
import App.Income.Items.Zaim (Entry(..), Account, Category)

data Config =
  Config
  { zaimEntryDay :: !Int,
    zaimAccount :: !Account,
    items :: ![ConfigPayment]
  }
  deriving (Show,Eq,Ord)

data ConfigPayment =
  ConfigPayment
  { paymentName :: !Text,
    zaimIncome :: !ConfigZaim,
    zaimPayment :: !ConfigZaim
  }
  deriving (Show,Eq,Ord)

data ConfigZaim =
  ConfigZaim
  { zaimName :: !Text,
    zaimCategory :: !Category,
    zaimSubcategory :: !Category
  }
  deriving (Show,Eq,Ord)

convert :: Config -> Report -> [Entry]
convert = undefined
