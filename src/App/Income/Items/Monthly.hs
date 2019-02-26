-- |
-- Module: App.Income.Items.Monthly
-- Description: Reader of monthly income report
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module App.Income.Items.Monthly
  () where

import Data.Foldable (toList)
import Data.List (elemIndex, break)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Text.CSV as CSV
import Text.Read (readMaybe)

type Yen = Integer

data Item =
  Item
  { itemName :: !Text,
    itemAmount :: !Yen
  }
  deriving (Show,Eq,Ord)

data Report =
  Report
  { reportYear :: !Integer,
    reportMonth :: !Int,
    reportItems :: ![Item]
  }
  deriving (Show,Eq,Ord)

readSJIS :: FilePath -> IO String
readSJIS = undefined

readReport :: FilePath -> IO Report
readReport = undefined

searchField :: String -> CSV.Record -> Maybe Int
searchField = elemIndex

(!?) :: [a] -> Int -> Maybe a
l !? i = if i >= 0 && i <= length l
         then Just $ l !! i
         else Nothing

readColumnPairs :: String -- ^ the heading field
                -> CSV.CSV
                -> [(CSV.Field, CSV.Field)]
readColumnPairs heading csv = go [] Nothing csv
  where
    go acc _ [] = acc
    go acc Nothing (record : rest) =
      go acc (searchField heading record) rest
    go acc mh@(Just h_index) (record : rest) =
      case record !? h_index of
        Nothing -> acc
        Just name -> 
          case record !? (h_index + 1) of
            Nothing -> go acc mh rest
            Just amount -> go ((name, amount) : acc) mh rest

readYearMonth :: Int -- ^ column index
              -> CSV.CSV
              -> Maybe (Integer, Int)
readYearMonth index csv = listToMaybe $ forRecord =<< csv
  where
    forRecord record = toList $ forField =<< (record !? index)
    forField field = (,) <$> readMaybe y_field <*> readMaybe m_field
      where
        (y_field, field_rest) = break (== '/') field
        m_field = drop 1 field_rest
