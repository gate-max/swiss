module Data.Read.Swiss (
    checkYear,
    readB,
    readC,
    readD, readDHuman, readDHuman2, readDMMMY, readDMY,
    readI, readInt, readIStr,
    readLargeNum,
    readMDY, readMMMDY,
    readS,
    readYMD,   
    ) where

import           Data.Char.Swiss (notAlpha, notDigit)
import           Data.Char (isAlpha, isDigit, toLower, toUpper)
import           Data.List (intercalate)
import           Data.List.Swiss (chunksOf, replaceOnce)
import           Data.Maybe (fromJust, fromMaybe, isJust)
import           Data.Time.Calendar (Day, fromGregorian)  --time
import           Text.Printf (printf)
import           Text.Read (readMaybe)



checkYear :: Maybe Integer -> Maybe Integer
checkYear x
    | i < 50 = (+ 2000) <$> x
    | i < 100 = (+ 1900) <$> x
    | otherwise = x
    where i = fromMaybe 101 x


-- | Test cases:
--  Just True :  from these input: True, "True", " TRUE", " 'true' ", Just True, Just "tURe", Right True", Right "\'true" 
-- Just False : from all input: False, "False", " 'FALSE", "faLSe ", Just False, Just "false ", Right "FALSe", Right "'false" 
-- Nothing :    from all input: Nothing, "Tr ue", "Fal se", Just "T RUE", Right "falsee" 
readB :: Show a => a -> Maybe Bool
readB x 
  | take 5 str `elem` ["just ", "right"] = readMaybe $ drop 5 str
  | otherwise = readMaybe str
  where str = replaceOnce "true" "True" . replaceOnce "false" "False" .
              map toLower . filter (`notElem` ['"','\'']) . show $ x


readC :: Show a => a -> Maybe Char
readC x
  | show x `elem` ["'\\''" , "\"'\"" , "Just '\\''" , "Just \"'\"" , "Right '\\''", "Right \"'\""] = Just '\''
  | show x `elem` ["'\"'" , "Just '\"'" , "Right '\"'" , "\"\\\"\"" , "Just \"\\\"\"" , "Right \"\\\"\"" ] = Just '"'
  | take 5 str == "Just " = readMaybe . addSingleQuote . drop 5 $ str
  | take 6 str == "Right " = readMaybe . addSingleQuote . drop 6 $ str
  | otherwise = readMaybe . addSingleQuote $ str
  where str = filter (`notElem` ['"','\'']) . show $ x
        addSingleQuote s = ('\'':s) ++ "\'"

-- do not incldue '\\' in readD
readD :: Show a => a -> Maybe Double
readD x
  | take 5 s == "Just "  = readD . drop 5 $ s
  | take 6 s == "Right " = readD . drop 6 $ s
  | take 1 s == "."      = readMaybe ('0': s)
  | take 2 s == "-."     = readMaybe . ("-0" ++) . drop 1 $ s
  | otherwise = readMaybe s
  where s = filter (`notElem` ['"', '\'', ',', '%', '+', '$', '¢', '£', '¥', '€']) . show $ x


-- | > readDHuman "212000000"      --> Just "212,000,000"
--   > readDHuman "212000000.2566" --> Just "212,000,000"
readDHuman :: Show a => a -> Maybe String
readDHuman x = integer_part
  where s            = printf "%.2f" <$> readD x
        integer_part = reverse . intercalate "," . chunksOf 3 . reverse . takeWhile (/= '.') <$> s


-- | > readDHuman "212000000"      --> Just "212,000,000.00"
--   > readDHuman "212000000.2566" --> Just "212,000,000.26"
readDHuman2 :: Show a => a -> Maybe String
readDHuman2 x = Just (++) <*> integer_part <*> decimal_part
  where s            = printf "%.2f" <$> readD x
        integer_part = reverse . intercalate "," . chunksOf 3 . reverse . takeWhile (/= '.') <$> s
        decimal_part = dropWhile (/= '.') <$> s 
-- | with 2 decimal place




-- | > readDMMMY "25 Dec 2017" --> Just 2017-12-25
readDMMMY :: Show a => a -> Maybe Day
readDMMMY x
  | isJust y && isJust m && isJust d = Just $ fromGregorian (fromJust y) (fromJust m) (fromJust d)
  | otherwise = Nothing
  where s = dropWhile notDigit . show $ x
        m = if l == 12 then Nothing else Just (l + 1)
        d = readInt . takeWhile isDigit $ s
        y = checkYear . readI . takeWhile isDigit . dropWhile notDigit .
            dropWhile isAlpha . dropWhile notAlpha $ s
        months = ["JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"]
        month_code = map toUpper . take 3 . dropWhile notAlpha $ s
        l = length . takeWhile (/= month_code) $ months  --Int


-- | > readDMY "25-12-2017" --> Just 2017-12-25
--   > readDMY "25-12-17" --> Just 2017-12-25
--   > readDMY "25/12/97" --> Just 1997-12-25
readDMY :: Show a => a -> Maybe Day
readDMY x
  | isJust y && isJust m && isJust d = Just $ fromGregorian (fromJust y) (fromJust m) (fromJust d)
  | otherwise = Nothing
  where s = dropWhile notDigit . show $ x
        d = readInt . takeWhile isDigit $ s
        m = readInt . takeWhile isDigit . drop 1 . dropWhile isDigit $ s
        y = checkYear . readI . takeWhile isDigit . drop 1 . dropWhile isDigit .
            drop 1 . dropWhile isDigit $ s




readI :: Show a => a -> Maybe Integer
readI x
  | take 5 s `elem` ["Just ", "Right"] = readMaybe . drop 5 $ s
  | otherwise = readMaybe s
  where s = filter (`notElem` ['\\', '"', '\'', ',', '%', '+', '$', '¢', '£', '¥', '€']) . show $ x



readInt :: Show a => a -> Maybe Int
readInt x
  | take 5 s `elem` ["Just ", "Right"] = readMaybe . drop 5 $ s
  | otherwise = readMaybe s
  where s = filter (`notElem` ['\\', '"', '\'', ',', '%', '+', '$', '¢', '£', '¥', '€']) . show $ x

      
readIStr :: Show a => a -> Maybe String
readIStr x = integer_part
                      where s = printf "%.0f" <$> readD x
                            integer_part = reverse . intercalate "," . chunksOf 3 . reverse <$> s



-- last character must be "M", "B" or "T"
-- | > readLargeNum "2M" --> Just 2000000.00
--   > readLargeNum "$3.2B" --> Just 3200000000.00
--   > readLargeNum (Just "¥3.2B") --> Just 3200000000.00
readLargeNum :: Show a => a -> Maybe Double
readLargeNum x
  | take 5 s == "Just "  = readLargeNum . drop 5 $ s
  | take 6 s == "Right " = readLargeNum . drop 6 $ s
  | lastchar == "M" = (*1000000) <$> readMaybe (init s)
  | lastchar == "B" = (*1000000000) <$> readMaybe (init s)
  | lastchar == "T" = (*1000000000000) <$> readMaybe (init s)
  | otherwise = Nothing
  where s = filter (`notElem` ['\\', '"', '\'', ',', '%', '+', '$', '¢', '£', '¥', '€']) . show $ x
        lastchar = map toUpper . take 1 . reverse $ s






-- | > readMDY (Right "12/25/2017") --> Just 2017-12-25
readMDY :: Show a => a -> Maybe Day
readMDY x
  | isJust y && isJust m && isJust d = Just $ fromGregorian (fromJust y) (fromJust m) (fromJust d)
  | otherwise = Nothing
  where s = dropWhile notDigit . show $ x
        m = readInt . takeWhile isDigit $ s
        d = readInt . takeWhile isDigit . drop 1 . dropWhile isDigit $ s
        y = checkYear . readI .  takeWhile isDigit . drop 1 . dropWhile isDigit .
            drop 1 . dropWhile isDigit $ s


-- | > readMMMDY "Dec 25, 2017" --> Just 2017-12-25
readMMMDY :: Show a => a -> Maybe Day
readMMMDY x
  | isJust y && isJust m && isJust d = Just $ fromGregorian (fromJust y) (fromJust m) (fromJust d)
  | otherwise = Nothing
  where s = dropWhile notAlpha . show $ x
        m = if l == 12 then Nothing else Just (l + 1)
        d = readInt . takeWhile isDigit .  dropWhile notDigit $ s
        y = checkYear . readI . takeWhile isDigit . dropWhile notDigit . dropWhile isDigit .
            dropWhile notDigit $ s
        months = ["JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"]
        month_code = map toUpper . take 3 $ s
        l = length . takeWhile (/= month_code) $ months  --Int


readS :: Show a => a -> Maybe String
readS x
  | s == "Nothing" = Nothing
  | take 5 s == "Just " = readMaybe . addDoubleQuote . drop 5 $ s
  | take 6 s == "Right " = readMaybe . addDoubleQuote . drop 6 $ s
  | otherwise = readMaybe . addDoubleQuote $ s
  where s = filter (`notElem` ['"', '\'', ',', '%', '+', '$', '¢', '£', '¥', '€', '\\', '[', ']']) . show $ x
        addDoubleQuote s = ('"':s) ++ ['"']



-- | > readYMD (Just "2017.12.25") --> Just 2017-12-25
readYMD :: Show a => a -> Maybe Day
readYMD x
  | isJust y && isJust m && isJust d = Just $ fromGregorian (fromJust y) (fromJust m) (fromJust d)
  | otherwise = Nothing
  where s = dropWhile notDigit . show $ x
        y = checkYear . readI . takeWhile isDigit $ s
        m = readInt . takeWhile isDigit . drop 1 . dropWhile isDigit $ s
        d = readInt . takeWhile isDigit . drop 1 . dropWhile isDigit .
            drop 1 . dropWhile isDigit $ s







