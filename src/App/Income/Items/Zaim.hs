-- |
-- Module: App.Income.Items.Zaim
-- Description: Data model for Zaim CSV
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module App.Income.Items.Zaim
  ( toRecord,
    toCSV,
    Entry(..),
    Transaction(..),
    Account
  ) where

import Data.Text (Text)
import Data.Time (Day)
import qualified Text.CSV as CSV

import App.Income.Items.Money (Yen)

type Account = Text

data Transaction = Payment !Account -- ^ Payment from the account
                 | Income !Account -- ^ Income into the account
                 | Transfer !Account !Account -- ^ Transfer from the 1st account into the 2nd account
                 deriving (Show,Eq,Ord)

data Entry =
  Entry
  { entryDate :: !Day,
    entryCategory :: !Text,
    entrySubcategory :: !Text,
    entryTransaction :: !Transaction,
    entryAmount :: !Yen,
    entryName :: !Text
  }
  deriving (Show,Eq,Ord)

toRecord :: Entry -> CSV.Record
toRecord = undefined

toCSV :: [Entry] -> CSV.CSV
toCSV = undefined
