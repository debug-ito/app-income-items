-- |
-- Module: App.Income.Items.Monthly
-- Description: Reader of monthly income report
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module App.Income.Items.Monthly
  ( readReport,
    Report(..),
    Item(..),
    Yen
  ) where

import Control.Exception.Safe (throwString)
import qualified Data.ByteString as BS
import Data.Char (isDigit)
import Data.Foldable (toList)
import Data.List (elemIndex, break)
import Data.Maybe (listToMaybe, catMaybes)
import Data.Text (Text, unpack, pack)
import qualified Data.Text.ICU.Convert as ICU
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

negateItem :: Item -> Item
negateItem item = item { itemAmount = negate $ itemAmount item }

unwrapEither :: Show a => Either a b -> IO b
unwrapEither (Right b) = return b
unwrapEither (Left a) = throwString $ show a

unwrapMaybe :: String -> Maybe a -> IO a
unwrapMaybe e Nothing = throwString e
unwrapMaybe _ (Just a) = return a

readSJIS :: FilePath -> IO String
readSJIS file = do
  conv <- ICU.open "Shift_JIS" Nothing
  unpack <$> ICU.toUnicode conv <$> BS.readFile file

-- | The raw CSV file contains a lot of '=' symbols in unexpected
-- places. This function sanitize the raw CSV text by removing them.
sanitizeRawCSV :: String -> String
sanitizeRawCSV raw = filter (/= '=') raw

readReport :: FilePath -> IO Report
readReport file = do
  csv <- unwrapEither =<< (CSV.parseCSV file <$> sanitizeRawCSV <$> readSJIS file)
  print csv
  (year, month) <- unwrapMaybe "Cannot find year/month field." $ readYearMonth 0 csv
  let incomes = readItems "支　給　内　容" csv
      payments = map negateItem $ readItems "引　去　内　容" csv
  return $ Report year month (payments ++ incomes)

searchField :: String -> CSV.Record -> Maybe Int
searchField = elemIndex

(!?) :: [a] -> Int -> Maybe a
l !? i = if i >= 0 && i < length l
         then Just $ l !! i
         else Nothing

parseItem :: (CSV.Field, CSV.Field) -> Maybe Item
parseItem (name, amount) = Item (pack name) <$> readMaybe (filter isDigit amount)

readItems :: String -- ^ the heading field
          -> CSV.CSV
          -> [Item]
readItems heading csv = catMaybes $ map parseItem $ readColumnPairs heading csv

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
        Just "" -> acc
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
