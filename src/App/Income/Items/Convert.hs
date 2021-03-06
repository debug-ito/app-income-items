{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
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

import Data.Aeson (FromJSON(..), genericParseJSON)
import qualified Data.Aeson as Aeson
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time (Day, fromGregorian)
import GHC.Generics (Generic)
import Text.Casing (quietSnake)

import App.Income.Items.Monthly (Report(..), Item(..))
import App.Income.Items.Zaim
  (Entry(..), Account, Category, Transaction(..))

data Config =
  Config
  { zaimEntryDay :: !Int,
    zaimAccount :: !Account,
    defaultZaimIncomeCategory :: !Category,
    items :: ![ConfigPayment]
  }
  deriving (Show,Eq,Ord,Generic)

instance FromJSON Config where
  parseJSON = genericParseJSON (aOptions fmod)
    where
      fmod = quietSnake

data ConfigPayment =
  ConfigPayment
  { paymentName :: !Text,
    zaimIncome :: !(Maybe ConfigZaim),
    zaimPayment :: !ConfigZaim
  }
  deriving (Show,Eq,Ord,Generic)

instance FromJSON ConfigPayment where
  parseJSON = genericParseJSON (aOptions fmod)
    where
      fmod = quietSnake

data ConfigZaim =
  ConfigZaim
  { zaimName :: !(Maybe Text),
    zaimCategory :: !Category,
    zaimSubcategory :: !Category
  }
  deriving (Show,Eq,Ord,Generic)

instance FromJSON ConfigZaim where
  parseJSON = genericParseJSON (aOptions fmod)
    where
      fmod = quietSnake . drop 4

aOptions :: (String -> String) -> Aeson.Options
aOptions fmod = Aeson.defaultOptions
                { Aeson.fieldLabelModifier = fmod
                }

convert :: Config -> Report -> [Entry]
convert conf report = reportItemToEntries conf date =<< reportItems report
  where
    date = zaimDay conf report

reportItemToEntries :: Config -> Day -> Item -> [Entry]
reportItemToEntries conf date item =
  case (mmatching_config, itemAmount item < 0) of
    (Just conf_pay, True) -> paymentEntries conf_pay
    _ -> []
  where
    mmatching_config = listToMaybe $ filter (\conf_pay -> itemName item == paymentName conf_pay) $ items conf
    paymentEntries conf_pay = [makeEntry True $ income_config, makeEntry False $ zaimPayment conf_pay]
      where
        income_config =
          case zaimIncome conf_pay of
            Just zconf -> zconf
            Nothing -> ConfigZaim { zaimName = Nothing,
                                    zaimCategory = defaultZaimIncomeCategory conf,
                                    zaimSubcategory = ""
                                  }
        makeEntry is_income zconf =
          Entry
          { entryDate = date,
            entryCategory = zaimCategory zconf,
            entrySubcategory = zaimSubcategory zconf,
            entryTransaction = if is_income
                               then Income $ zaimAccount conf
                               else Payment $ zaimAccount conf,
            entryAmount = abs $ itemAmount item,
            entryName = entry_name
          }
          where
            entry_name = case (zaimName zconf, is_income) of
                           (Just n, _) -> n
                           (Nothing, False) -> itemName item
                           (Nothing, True) -> itemName item <> " 給与控除分"

zaimDay :: Config -> Report -> Day
zaimDay c r = fromGregorian y m d
  where
    y = reportYear r
    m = reportMonth r
    d = zaimEntryDay c
