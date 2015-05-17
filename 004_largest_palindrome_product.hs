-- A palindromic number reads the same both ways. The largest palindrome made from 
-- the product of two 2-digit numbers is 9009 = 91 × 99.
-- 
-- Find the largest palindrome made from the product of two 3-digit numbers.

largestPalindrome :: (Integral a) => a
largestPalindrome = maximum palindromeList

-- creates a List of all palindromes that are a product of two three digit numbers
-- lists also the respective factors
palindromeListLarge :: (Integral a) => [(a,a,a)]
palindromeListLarge = [(x, y, x*y) | x <- [100..999], y <- [x..999], checkPalindrome (x*y)]

-- same as palindromeListLarge but without respective factors
palindromeList :: (Integral a) => [a]
palindromeList = [x*y | x <- [100..999], y <- [x..999], checkPalindrome (x*y)]

-- takes a number and returns true if it is a palindrome. 
-- Single digit Numbers are return also true
checkPalindrome :: (Integral a) => a -> Bool
checkPalindrome n = checkPalindrome' (digitsToList n)
  
checkPalindrome' :: (Integral a) => [a] -> Bool
checkPalindrome' nList
  | nList == [] = True
  | nFirst == nLast = checkPalindrome' nShort
  | otherwise = False
  where nFirst = head nList
        nLast = last nList
        nShort = take (length nList -2) (tail nList)  

-- converts an Integral to a list
digitsToList :: (Integral a) => a -> [a]
digitsToList n = digitsToList' n []

digitsToList' :: (Integral a) => a -> [a] -> [a]
digitsToList' n nList
  | n < 10 = n : nList
  | otherwise = digitsToList' (div n 10) ((mod n 10) : nList)  
  
main :: IO()
main = putStrLn $ show largestPalindrome

-- unused:

-- extract first digit in number
extFirstDigit :: (Integral a) => a -> a
extFirstDigit n
  | n > 9 = extFirstDigit (div n 10)
  | otherwise = n

-- delete first digit in number
delFirstDiget :: (Integral a) => a -> a
delFirstDiget n = mod n (10^(llength - 1))
  where list = digitsToList n
        llength = length list
