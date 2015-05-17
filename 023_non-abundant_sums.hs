-- A perfect number is a number for which the sum of its proper divisors is exactly equal to 
-- the number. For example, the sum of the proper divisors of 28 would be 
-- 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
-- 
-- A number n is called deficient if the sum of its proper divisors is less than n and it is 
-- called abundant if this sum exceeds n.
-- 
-- As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that 
-- can be written as the sum of two abundant numbers is 24. By mathematical analysis, it can 
-- be shown that all integers greater than 28123 (wiki, oeis: 20161) can be written as the sum of two abundant 
-- numbers. However, this upper limit cannot be reduced any further by analysis even though 
-- it is known that the greatest number that cannot be expressed as the sum of two abundant 
-- numbers is less than this limit.
-- 
-- Find the sum of all the positive integers which cannot be written as the sum of two 
-- abundant numbers

-- Find: sum [x | x <- [1..28123] isSumOfTwoAbundandNumbers x == False]
-- additional info: smallest abundant number prime to 2 and 3 is 5391411025
-- if n is abundant n*k is also abundant
-- the greatest number thats a sum of 2 abundant numbers is 20161

import Data.Bits
import Data.List
  
main :: IO()
-- main = print $ sum $ crossOutSums [1..20161] (listAbundandNumbers' 20161)
main = print $ length $ crossOutSums [1..1000] (listAbundandNumbers' 1000)
-- main = print $ listAbundandNumbers' 1000
-- main = print $ sum $ listAbundandNumbers' 20161

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- TEST START
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

-- returns a list of all numbers from list 1 that are not sums of numbers from list 2
crossOutSums :: (Integral a) => [a] -> [a] -> [a]
crossOutSums list summands = crossOutSumsAux list summands 0

crossOutSumsAux :: (Integral a) => [a] -> [a] -> Int -> [a]
crossOutSumsAux list summands i
  | i >= (length summands -1) = list
  | otherwise = crossOutSumsAux (list \\ sumList) summands (i+1)
  where s = summands !! i
        sumList = map (\x -> x + s) summands

-- crossOutSumsAux :: (Integral a) => [a] -> [a] -> Int -> [a]
-- crossOutSumsAux list summands i
--   | i >= (length summands -1) = list
--   | otherwise = crossOutSumsAux (list \\ sumList) summands (i+1)
--   where s = summands !! i
--         sumList = map (\x -> x + s) summands
        
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- TEST END
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


-- [Optimized] lists all abundant number between 1 and n (works only safe for n < 2.14 Bil)
listAbundandNumbers' :: Int -> [Int]
listAbundandNumbers' n = [ x | x <- [12..n], (even' x || mod x 3 == 0),  isAbundandNumber x]

-- returns true if n is less than the sum of its proper divisors
isAbundandNumber :: (Integral a) => a -> Bool
isAbundandNumber n = sumProperDivisors n > n

-- returns sum of proper divisors of n 
sumProperDivisors :: (Integral a) => a -> a
sumProperDivisors 1 = 1
sumProperDivisors n = sum $ listProperDivisors n

-- lists all proper Divisors of n
listProperDivisors :: (Integral a) => a -> [a]
listProperDivisors 1 = [1]
listProperDivisors n = 1 : (filter (\x -> x /= n) $ delDuplicates $ map product (subsets $ primefactors n))

-- deletes all multiple instances in a list
delDuplicates :: (Integral a) => [a] -> [a]
delDuplicates l = delDuplicatesAux l []

delDuplicatesAux :: (Integral a) => [a] -> [a] -> [a]
delDuplicatesAux l retL
  | l == [] = retL
  | elem headL retL = delDuplicatesAux tailL retL
  | otherwise = delDuplicatesAux tailL (headL : retL)
  where tailL = tail l
        headL = head l
        
-- lists all prime factors of n
primefactors :: (Integral a) => a -> [a]
primefactors n = primefactorsAux n 2 []

primefactorsAux :: (Integral a) => a -> a -> [a] -> [a]
primefactorsAux n divisor factors
  | n == 1 = factors
  | (mod n divisor) == 0 = primefactorsAux (div n divisor) (divisor) (divisor:factors)
  | otherwise = primefactorsAux n (divisor+1) factors


-- returns a list of all subsets of a given list represented as lists
subsets :: (Integral a) => [a] -> [[a]]
subsets list
  | length list == 1 = [list]
  | otherwise = nextSubsets ++ ( concatAll nextSubsets (head list) ) ++ [[head list]]
  where nextSubsets = subsets (tail list)

-- takes a list of list and concatinates an given n to each list
concatAll :: (Integral a) => [[a]] -> a -> [[a]]
concatAll list n = [ n : x | x <- list]

-- optimized even
even' :: Int -> Bool
even' n
  | n .&. 1 == 0 = True
  | otherwise = False
  