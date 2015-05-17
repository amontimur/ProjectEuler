-- You are given the following information, but you may prefer to do some research 
-- for yourself.
-- 
-- 1 Jan 1900 was a Monday.
-- Thirty days has September,
-- April, June and November.
-- All the rest have thirty-one,
-- Saving February alone,
-- Which has twenty-eight, rain or shine.
-- And on leap years, twenty-nine.
-- A leap year occurs on any year evenly divisible by 4, but not on a century 
-- unless it is divisible by 400.
-- 
-- How many Sundays fell on the first of the month during the twentieth century 
-- (1 Jan 1901 to 31 Dec 2000)?

-- returns the next day
succDay :: String -> String
succDay d
  | d == "Monday"    = "Tuesday"
  | d == "Tuesday"   = "Wednesday"
  | d == "Wednesday" = "Thursday"
  | d == "Thursday"  = "Friday"
  | d == "Friday"    = "Saturday"
  | d == "Saturday"  = "Sunday"
  | d == "Sunday"    = "Monday"

-- returns the date of the next Day
nextDate :: (Int,Int,Int,String) -> (Int,Int,Int,String)
nextDate (d,m,y,s)
  | d == 31 && m == 12                  = (1,1,succ y, succDay s)
  | d == 31 && elem m [1,3,5,7,8,10]    = (1, succ m, y, succDay s)
  | d == 30 && elem m [4,6,9,11]        = (1, succ m, y, succDay s)
  | d == 29 && m == 2                   = (1, 3, y, succDay s)
  | d == 28 && m == 2 && mod y 4 /= 0 && ( mod y 100 == 0 || mod y 400 /= 0) = (1,3,y,succDay s)
  | otherwise = (succ d, m, y, succDay s)
  
-- returns the date one month from the given date
nextMonth :: (Int,Int,Int) -> (Int,Int,Int)
nextMonth (d,m,y)
  | m == 12     = (d,1, succ y)
  | otherwise   = (d, succ m, y)
  
-- takes a start and a end date in form (day,month,year) and returns all dates within 
-- the boundaries
dateList' :: (Int,Int,Int,String) -> (Int,Int,Int,String) -> [(Int,Int,Int,String)]
dateList' start end = dateList'Aux start end []

dateList'Aux :: (Int,Int,Int,String) -> (Int,Int,Int,String) -> [(Int,Int,Int,String)]-> [(Int,Int,Int,String)]
dateList'Aux start end list
  | start == end = end : list
  | otherwise = dateList'Aux (nextDate start) end (start : list)
  
numberToDay :: Int -> String
numberToDay d
  | d == 0  = "Sunday"
  | d == 1  = "Monday"
  | d == 2  = "Tuesday"
  | d == 3  = "Wednesday"
  | d == 4  = "Thursday"
  | d == 5  = "Friday"
  | d == 6  = "Saturday"
  
-- returns the day of the week of the given date using gaussians formula
dayOfTheWeekGaussian :: (Int,Int,Int) -> Int
dayOfTheWeekGaussian (d,m,y) = mod (d + floor (2.6 * fromIntegral m' - 0.2) + y' + ( div y' 4) + ( div c 4) - 2 * c) 7
  where y' = if m > 2 then mod y 100 else mod (y-1) 100
        c  = if m > 2 then div y 100 else div (y-1) 100
        m' = if m > 2 then m - 2 else m + 10
        
countingSundays :: (Int,Int,Int) -> (Int,Int,Int) -> Int
countingSundays start end = countingSundaysAux start end 0

countingSundaysAux :: (Int,Int,Int) -> (Int,Int,Int) -> Int -> Int
countingSundaysAux start end counter
  | (compareDates start end) > 0    = counter
  | dayOfTheWeekGaussian start == 0 = countingSundaysAux (nextMonth start) end (counter +1)
  | otherwise = countingSundaysAux (nextMonth start) end counter
  
  
compareDates :: (Int,Int,Int) -> (Int,Int,Int) -> Int
compareDates (d1,m1,y1) (d2,m2,y2)
  | y1 > y2 =  1
  | y1 < y2 = -1
  | m1 > m2 =  1
  | m1 < m2 = -1
  | d1 > d2 =  1
  | d1 < d2 = -1
  | otherwise = 0
  
main :: IO()
main = putStrLn $ show countingSundays (1,1,1901) (1,12,2000)