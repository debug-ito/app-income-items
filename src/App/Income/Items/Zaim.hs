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
    Account,
    Category
  ) where

import Data.Text (Text, unpack)
import Data.Time (Day, formatTime, defaultTimeLocale)
import qualified Text.CSV as CSV

import App.Income.Items.Money (Yen)

type Account = Text

type Category = Text

data Transaction = Payment !Account -- ^ Payment from the account
                 | Income !Account -- ^ Income into the account
                 | Transfer !Account !Account -- ^ Transfer from the 1st account into the 2nd account
                 deriving (Show,Eq,Ord)

data Entry =
  Entry
  { entryDate :: !Day,
    entryCategory :: !Category,
    entrySubcategory :: !Category,
    entryTransaction :: !Transaction,
    entryAmount :: !Yen,
    entryName :: !Text
  }
  deriving (Show,Eq,Ord)

toRecord :: Entry -> CSV.Record
toRecord e =
  [date, category, subcategory, memo, pay_from, income_to, pay_amount, income_amount, transfer_amount]
  where
    date = formatTime defaultTimeLocale "%Y-%m-%d" $ entryDate e
    category = unpack $ entryCategory e
    subcategory = unpack $ entrySubcategory e
    memo = unpack $ entryName e
    amount = show $ entryAmount e
    (pay_from, income_to, pay_amount, income_amount, transfer_amount) =
      case entryTransaction e of
        Payment a -> (unpack a, "", amount, "", "")
        Income a -> ("", unpack a, "", amount, "")
        Transfer from to -> (unpack from, unpack to, "", "", amount)
    
toCSV :: [Entry] -> CSV.CSV
toCSV es = title : map toRecord es
  where
    title = [ "date", "category", "subcategory", "memo", "pay_from", "income_to",
              "pay_amount", "income_amount", "transfer_amount"
            ]
