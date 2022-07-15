module Data.Time.Swiss (
    addTradingDays,
    dayToStr,
    dayToUnixtime,
    exchangeHolidays,
    federalHolidays,
    
    getDay, getMonth, getToday, getTradingDay, getTradingDayUTC, getYear,
    holidayChristmas, holidayColumbus, holidayGoodFriday, holidayIndependence, holidayLabor, holidayMartinLuther,
    holidayMemorial, holidayNewYears, holidayThanksgiving, holidayVeterans,
    isExchangeHoliday, isFederalHoliday, isTradingDay,
    isWednesday, isThursday, isFriday, isSaturday, isSunday, isMonday, isTuesday,
    isWeekday, isWeeklyClose,
    lastTuesday, lastMonday, lastSunday, lastSaturday, lastFriday, lastThursday, lastWednesday,
    mkDay,
    nextTradingDay,
    nextWednesday, nextTuesday, nextMonday, nextSunday, nextSaturday, nextFriday, nextThursday,
    notExchangeHoliday, notFederalHoliday, notTradingDay,
    notWednesday, notThursday, notFriday, notSaturday, notSunday, notMonday, notTuesday,
    notWeekday, notWeeklyClose, 
    previousTradingDay,
    showGreg,
    timer, timerIO, timerPure,
    unixtimeToDay,
    ) where
    

import           Data.Time

import           Data.Char (isDigit) 

import           Data.Time.Calendar (Day(toModifiedJulianDay), addDays, fromGregorian, toGregorian) --time
import           Data.Time.Clock (getCurrentTime, UTCTime(utctDay))
import           Data.Time.Calendar.Easter (gregorianEaster)


addTradingDays :: Integral i => i -> Day -> Day
addTradingDays n day
  | n == 0 = day
  | n > 0 = nextTradingDay $ addTradingDays (n - 1) day 
  | otherwise = previousTradingDay $ addTradingDays (n + 1) day  -- n < 0

-- | > dayToString (mkDay 2016 3 14) --> "20160314"
dayToStr :: Day -> String
dayToStr = filter (isDigit) . show

-- | diffDays (mkDay 1970 1 1) (mkDay 1858 11 17) is 40587; 1 day has 86400 seconds
-- compare: unixtimeToDay
dayToUnixtime :: Day -> Integer
dayToUnixtime day = (toModifiedJulianDay day - 40587) * 86400

-- | this is actually CME Exchange Holiday, which includes Good Friday, while Federal holiday not
exchangeHolidays :: Integer -> [Day]
exchangeHolidays year = federalHolidays year ++ [holidayGoodFriday year]



-- | Federal holidays do not include Easter
--  New Years Day might be Saturady and falls into 31 Dec, see year 2010 
-- https://www.opm.gov/policy-data-oversight/snow-dismissal-procedures/federal-holidays/
federalHolidays :: Integer -> [Day]
federalHolidays year
  | nextYearJan1isSat = thisYearFederalHolidays ++ [mkDay year 12 31] 
  | otherwise         = thisYearFederalHolidays  
  where nextYearJan1isSat = isSaturday $ mkDay (year + 1) 1 1 
        thisYearFederalHolidays =  [holidayNewYears year, holidayMartinLuther year, holidayWashington year
            , holidayMemorial year, holidayIndependence year, holidayLabor year
            , holidayColumbus year, holidayVeterans year, holidayThanksgiving year, holidayChristmas year]


  
getDay :: Day -> Int
getDay day = d where (_,_,d) = toGregorian day

getMonth :: Day -> Int
getMonth day = m where (_,m,_) = toGregorian day 

-- | This is the current GMT day
getToday :: IO Day
getToday = utctDay <$> getCurrentTime

getTradingDay :: IO Day
getTradingDay = previousTradingDay <$> getToday

getTradingDayUTC :: IO (Day, UTCTime)
getTradingDayUTC = do
    utc <- getCurrentTime
    let trading_day = previousTradingDay . utctDay $ utc
    return (trading_day, utc) 

getYear :: Day -> Integer
getYear day = y where (y,_,_) = toGregorian day




-- | New Year's Day is fixed at January 1st, falls to Dec 31 if Saturday
holidayNewYears :: Integer -> Day
holidayNewYears year
  | isSaturday jan1 = pred $ jan1
  | isSunday jan1   = mkDay year 1 2
  | otherwise       = jan1
  where jan1 = mkDay year 1 1
  
-- | Martin Luther Day is the third Monday in January
holidayMartinLuther :: Integer -> Day
holidayMartinLuther year = nextMonday (mkDay year 1 14)

-- | Presidents' Day is the third Monday in February
holidayWashington :: Integer -> Day
holidayWashington year  = nextMonday (mkDay year 2 14)

-- | Good Friday is observed by CME, though it is not a US Federal Holiday
holidayGoodFriday :: Integer -> Day
holidayGoodFriday year = lastFriday $ gregorianEaster year


-- | Memorial Day is the last Monday in May
holidayMemorial :: Integer -> Day
holidayMemorial year  = lastMonday (mkDay year 6 1)

-- | Independence Day is fixed at July 4th
holidayIndependence :: Integer -> Day
holidayIndependence year
  | isSaturday july4 = mkDay year 7 3
  | isSunday july4   = mkDay year 7 5
  | otherwise        = july4
  where july4 = mkDay year 7 4
  
-- | Labor Day is the first Monday in September
holidayLabor :: Integer -> Day
holidayLabor year  = nextMonday (mkDay year 8 31)

-- | Columbus Day is the second Monday in October
holidayColumbus :: Integer -> Day
holidayColumbus year  = nextMonday (mkDay year 10 7)

-- | Veterans Day is fixed at November 11th
holidayVeterans :: Integer -> Day
holidayVeterans year
  | isSaturday nov11 = mkDay year 11 10
  | isSunday nov11   = mkDay year 11 12
  | otherwise        = nov11
  where nov11 = mkDay year 11 11
  
-- | Thanksgiving Day is the fourth Thursday in November
holidayThanksgiving :: Integer -> Day
holidayThanksgiving year  = nextThursday (mkDay year 11 21)

-- | Christmas Day is fixed at December 25th
holidayChristmas :: Integer -> Day
holidayChristmas year
  | isSaturday dec25 = mkDay year 12 24
  | isSunday dec25   = mkDay year 12 26
  | otherwise        = dec25
  where dec25 = mkDay year 12 25



isExchangeHoliday :: Day -> Bool
isExchangeHoliday day = day `elem` (exchangeHolidays $ getYear day)


-- | New Years Day might be Saturady and falls into 31 Dec, see year 2010 
isFederalHoliday :: Day -> Bool
isFederalHoliday day = day `elem` (federalHolidays $ getYear day)


isTradingDay :: Day -> Bool
isTradingDay day = not (isSunday day || isSaturday day || isExchangeHoliday day)



isWednesday,isThursday,isFriday,isSaturday,isSunday,isMonday,isTuesday :: Day -> Bool
[isWednesday,isThursday,isFriday,isSaturday,isSunday,isMonday,isTuesday] = [isDay i | i <- [0 .. 6]]
    where isDay :: Integer -> Day -> Bool
          isDay i day = toModifiedJulianDay day `mod` 7 == i

isWeekday :: Day -> Bool
isWeekday day = not (isSaturday day || isSunday day)


isWeeklyClose :: Day -> Bool
isWeeklyClose day
    | isFriday day && notExchangeHoliday day = True
    | isThursday day && isExchangeHoliday tomorrow = True
    | otherwise = False
    where tomorrow = succ day


lastTuesday,lastMonday,lastSunday,lastSaturday,lastFriday,lastThursday,lastWednesday :: Day -> Day
[lastTuesday,lastMonday,lastSunday,lastSaturday,lastFriday,lastThursday,lastWednesday] = [lastDay i | i <- [0 .. 6]]
    where lastDay :: Integer -> Day -> Day
          lastDay i day =  addDays ((negate $ (toModifiedJulianDay day + i) `mod` 7) - 1) day


mkDay :: Integer -> Int -> Int -> Day
mkDay = fromGregorian


nextTradingDay :: Day -> Day
nextTradingDay day
  | isTradingDay tomorrow = tomorrow  
  | otherwise = nextTradingDay tomorrow
  where tomorrow = succ day



nextWednesday,nextTuesday,nextMonday,nextSunday,nextSaturday,nextFriday,nextThursday :: Day -> Day
[nextWednesday,nextTuesday,nextMonday,nextSunday,nextSaturday,nextFriday,nextThursday] = [nextDay i | i <- [0 .. 6]]
    where nextDay :: Integer -> Day -> Day
          nextDay i day = addDays (7 - (toModifiedJulianDay day + i) `mod` 7) day  

  


notExchangeHoliday :: Day -> Bool
notExchangeHoliday = not . isExchangeHoliday


notFederalHoliday :: Day -> Bool
notFederalHoliday = not . isFederalHoliday 


notTradingDay :: Day -> Bool
notTradingDay = not . isTradingDay 

notWednesday, notThursday, notFriday, notSaturday, notSunday, notMonday, notTuesday :: Day -> Bool
[notWednesday, notThursday, notFriday, notSaturday, notSunday, notMonday, notTuesday] = [notDay i | i <- [0 .. 6]]
    where notDay :: Integer -> Day -> Bool
          notDay i day = toModifiedJulianDay day `mod` 7 /= i


notWeekday :: Day -> Bool
notWeekday day = isSaturday day || isSunday day



notWeeklyClose :: Day -> Bool
notWeeklyClose = not . isWeeklyClose 



previousTradingDay :: Day -> Day
previousTradingDay day
  | isTradingDay yesterday = yesterday  
  | otherwise = previousTradingDay yesterday
  where yesterday = pred day

-- | compare showGregorian
-- > dayToString (mkDay 2016 3 14) --> "20160314"
showGreg :: Day -> String
showGreg = filter (isDigit) . show


-- | use 'timer' for inline coding
--
--   > ghci> :m + Dimsum.Control.Tools
--   > ghci> :m + Network.HTTP.Conduit
--   > ghci Dimsum.Control.Tools Network.HTTP.Conduit> timer $ take 16 . show <$> simpleHttp "http://www.yahoo.com"
--   > Output: ("\"<!DOCTYPE html>",0.973127378s)
timer :: IO t -> IO (t, NominalDiffTime)
timer action = do
  start <- getCurrentTime
  result <- action
  end <- getCurrentTime
  let elapsed_time = end `diffUTCTime` start
  return (result, elapsed_time)


-- | use 'timerIO' in ghci
-- > ghci> timerIO . print $ sum [1..1000000]
-- > Output:
-- > 500000500000
-- > 0.592073825s elapsed.
timerIO :: IO a -> IO ()
timerIO action = do
  start <- getCurrentTime
  action
  end <- getCurrentTime
  putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."


-- | use 'timerPure' in ghci
--
-- Example 1:
--
-- > ghci> timerPure $ sum [1..1000000]
-- > Output:
-- > 500000500000
-- > 0.592073825s elapsed.
timerPure :: Show a => a -> IO ()
timerPure = timerIO . print


-- 1 day has 86400 seconds
-- see also dayToUnixtime function
unixtimeToDay :: Integer -> Day
unixtimeToDay i = addDays (i `div` 86400) (mkDay 1970 1 1) 


