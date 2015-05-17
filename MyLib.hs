module MyLib 
( aksTest
, deleteMultiples
, even'
, fastExp
, fibonacci
, intLog2
, perfectPowerTest
, sieveOfEratosthenes
, sqrt'
) where

import Data.Bits

-- deterministic test for primiality in O()
aksTest :: Integer -> Bool
aksTest n
  | perfectPowerTest n /= (-1,-1) = False
  | otherwise = aksTestAux n r l 1
  where r = aksFindR
        l = undefined

aksTestAux = undefined

aksFindR = undefined
-- deletes all multiples of n from a given list
deleteMultiples ::  (Integral a) => a -> [a] -> [a]
deleteMultiples n list = [x | x <- list, (mod x n) /= 0]

-- optimized even for Int
even' :: Int -> Bool
even' n
  | n .&. 1 == 0 = True
  | otherwise = False

-- fast version for y = a^n
fastExp :: Integer -> Integer -> Integer
fastExp 0 _ = 0
fastExp a 1 = a
fastExp a n = fastExpAux a n 1

fastExpAux :: Integer -> Integer -> Integer -> Integer
fastExpAux b n y
  | n == 0        = y
  | n .&. 1 == 1  = fastExpAux (b*b) (shiftR n 1) (y*b)
  | otherwise     = fastExpAux (b*b) (shiftR n 1) y

-- calculates the n-th fibonacci number
fibonacci :: (Integral a) => a -> a
fibonacci 1 = 1
fibonacci 2 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

intLog2 :: Integer -> Integer
intLog2 n = floor $ logBase 2 (fromInteger n)

-- if n is a perfect power the fuction returns m^e = n else (-1,-1)
perfectPowerTest :: Integer -> (Integer,Integer)
perfectPowerTest n = perfectPowerTestAux n 2 (floor $ logBase 2 (fromInteger n)) 2 n

perfectPowerTestAux :: Integer -> Integer -> Integer -> Integer -> Integer -> (Integer,Integer)
perfectPowerTestAux n e maxE m1 m2
  | e > maxE    = (-1,-1)   -- not a perfect power
  | m1 > m2     = perfectPowerTestAux n (e+1) maxE 2 n
  | mToE == n   = (m,e)
  | mToE >  n   = perfectPowerTestAux n e maxE m1 (m-1)
  | otherwise   = perfectPowerTestAux n e maxE (m+1) m2
  where m = div (m1 + m2) 2 -- div truncates towards neg inf
        mToE = fastExp m e

-- returns all prime numbers between 2 and n
sieveOfEratosthenes :: (Integral a) => a -> [a]
sieveOfEratosthenes n = sieveOfEratosthenesAux (sqrt' n) [2..n] []

sieveOfEratosthenesAux :: (Integral a) => a -> [a] -> [a] -> [a]
sieveOfEratosthenesAux endN list primes
  | head list > endN = primes ++ list
  | otherwise = sieveOfEratosthenesAux endN (deleteMultiples (head list) list) (head list : primes)

-- square root for integrals
sqrt' :: (Integral a) => a -> a
sqrt' n = floor (sqrt ( fromIntegral n))


