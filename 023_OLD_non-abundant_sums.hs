-- A perfect number is a number for which the sum of its proper divisors is exactly equal to 
-- the number. For example, the sum of the proper divisors of 28 would be 
-- 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
-- 
-- A number n is called deficient if the sum of its proper divisors is less than n and it is 
-- called abundant if this sum exceeds n.
-- 
-- As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that 
-- can be written as the sum of two abundant numbers is 24. By mathematical analysis, it can 
-- be shown that all integers greater than 28123 (wiki: 20161) can be written as the sum of two abundant 
-- numbers. However, this upper limit cannot be reduced any further by analysis even though 
-- it is known that the greatest number that cannot be expressed as the sum of two abundant 
-- numbers is less than this limit.
-- 
-- Find the sum of all the positive integers which cannot be written as the sum of two 
-- abundant numbers

-- Find: sum [x | x <- [1..28123] isSumOfTwoAbundandNumbers x == False]
-- additional info: smallest abundant number prime to 2 and 3 is 5391411025
-- if n is abundant n*k is also abundant

-- Idea: list all abundant numbers from 12 to 20161. Find all sums. 
-- then either sum [1..28123] - sum [abundantNumbers]
-- or filter list and then sum

import Data.Bits
  
-- main :: IO()
-- main = print $ sumNonAbudandSumNumbers 28123
-- main = print $ sumAllTwoSums [1..1000]

-- idea :: (Integral a) => a
idea = ((sum [1..28123]) - (sumAllTwoSums $ listAbundandNumbers' 28123))
  
-- sumNonAbudandSumNumbers :: Int -> Int
sumNonAbudandSumNumbers n = length $ delDuplicates [x+y | x <- abundantNumbers, y <- abundantNumbers, y > x]
  where abundantNumbers = listAbundandNumbers n
    
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

-- listAllTwoSums' :: [Int] -> [Int]
-- listAllTwoSums' 
  
-- takes a list of integers and returns the list of all possible sums of two list elements
listAllTwoSums :: (Integral a) => [a] -> [a]
listAllTwoSums list = listAllTwoSumsAux list []

listAllTwoSumsAux :: (Integral a) => [a] -> [a] -> [a]
listAllTwoSumsAux list retL
  | list == [] = retL
  | otherwise = listAllTwoSumsAux (tail list) (retL ++ (map (\x -> x + (head list)) (tail list)))
  
-- takes a list of integers and returns the sum of the list of all possible sums of sums of 
-- two elements
sumAllTwoSums :: (Integral a) => [a] -> a
sumAllTwoSums list = sumAllTwoSumsAux list 0

sumAllTwoSumsAux :: (Integral a) => [a] -> a -> a
sumAllTwoSumsAux list res
  | list == [] = res
  | otherwise = sumAllTwoSumsAux (tail list) ((sum $ delDuplicates $ map (\x -> x + (head list)) (tail list)) + res)
  
  
-- unused:

-- returns the sum of all numbers that can not be made by the addition of two abundant numbers
sumNonAbudandSumNumbersBAD :: (Integral a) => a
sumNonAbudandSumNumbersBAD = sum [x | x <- [1..28123], (isSumOfTwoAbundandNumbersAux x abundantNumbers) == False]
  where abundantNumbers = listAbundandNumbers 28123

-- returns the sum of all numbers that can be made by the addition of two abundant numbers
sumAbudandSumNumbersBAD :: (Integral a) => a
sumAbudandSumNumbersBAD = sum [x | x <- [1..28123], isSumOfTwoAbundandNumbersAux x abundantNumbers]
  where abundantNumbers = listAbundandNumbers 28123

-- optimized odd
odd' :: Int -> Bool
odd' n
  | n .&. 1 == 1 = True
  | otherwise = False
  
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted 

-- returns true if n is a sum of two abundant numbers
isSumOfTwoAbundandNumbers :: (Integral a) => a -> Bool
isSumOfTwoAbundandNumbers n = isSumOfTwoAbundandNumbersAux n (listAbundandNumbers n)

isSumOfTwoAbundandNumbersAux :: (Integral a) => a -> [a] -> Bool
isSumOfTwoAbundandNumbersAux n list = 0 /= length [(x,y) | x <- list, y <- list, x <= y, x+y == n]

-- lists all abundant number between 1 and n
listAbundandNumbers :: (Integral a) => a -> [a]
listAbundandNumbers n = [ x | x <- [1..n], isAbundandNumber x]
