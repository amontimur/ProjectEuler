import Data.Bits

-- The prime factors of 13195 are 5, 7, 13 and 29.
-- 
-- What is the largest prime factor of the number 600851475143 ?


-- lists all prime factors of n
primefactors :: (Integral a) => a -> [a]
primefactors n = primefactorsAux n 2 []

primefactorsAux :: (Integral a) => a -> a -> [a] -> [a]
primefactorsAux n divisor factors
  | n == 1 = factors
  | (mod n divisor) == 0 = primefactorsAux (div n divisor) (divisor) (divisor:factors)
  | otherwise = primefactorsAux n (divisor+1) factors
  
-- main :: IO()
-- main = putStrLn $ show $ primefactors 600851475143
-- main = putStrLn $ show $ primefactors 79904872159354901403735136257271915903199354993162141876782616770989250284251



-- unused:

-- square root for integrals
sqrt' :: (Integral a) => a -> a
sqrt' n = floor (sqrt ( fromIntegral n))

-- deletes all multiples of n from a given list
deleteMultiples ::  (Integral a) => a -> [a] -> [a]
deleteMultiples n list = [x | x <- list, (mod x n) /= 0]

-- returns all prime numbers between 2 and n
sieveOfEratosthenes :: (Integral a) => a -> [a]
sieveOfEratosthenes n = sieveOfEratosthenesAux (sqrt' n) [2..n] []

sieveOfEratosthenesAux :: (Integral a) => a -> [a] -> [a] -> [a]
sieveOfEratosthenesAux endN list primes
  | head list > endN = primes ++ list
  | otherwise = sieveOfEratosthenesAux endN (deleteMultiples (head list) list) (head list : primes)


listTilSqrt :: (Integral a) => a -> [a]
listTilSqrt n = [2..(floor (sqrt (fromIntegral n)))]
  

isPrimeFermat :: (Integral a) => a -> Bool
isPrimeFermat n 
  | mod n 2 == 0 = False
  | otherwise = isPrimeFermat' n 2

isPrimeFermat' :: (Integral a) => a -> a -> Bool
isPrimeFermat' n base
  | mod n base == 0 = isPrimeFermat' n (base+1)
  | otherwise       = (mod (base^(n-1)) n) == 1

lastPrimeFactor :: (Integral a) => a -> a
lastPrimeFactor maxN = lastPrimeFactor' maxN (floor (sqrt ( fromIntegral maxN)))

lastPrimeFactor' :: (Integral a) => a -> a -> a
lastPrimeFactor' maxN n
  | n == 1            = maxN
  | (mod maxN n) == 0 = lastPrimeFactor' n (floor (sqrt ( fromIntegral n)))
  | otherwise         = lastPrimeFactor' maxN (n-1)
