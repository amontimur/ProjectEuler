-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
-- 
-- What is the 10 001st prime number?

-- returns the n'th prime number
sieveOfEratosthenes2 :: (Integral a) => a -> a
sieveOfEratosthenes2 n = sieveOfEratosthenes2' n [2..]

sieveOfEratosthenes2' :: (Integral a) => a -> [a] -> a
sieveOfEratosthenes2' pIndex list
  | pIndex == 1 = head list
  | otherwise = sieveOfEratosthenes2' (pIndex - 1) (deleteMultiples (head list) list)

-- deletes all multiples of n from a given list
deleteMultiples ::  (Integral a) => a -> [a] -> [a]
deleteMultiples n list = [x | x <- list, (mod x n) /= 0]

-- square root for integrals
sqrt' :: (Integral a) => a -> a
sqrt' n = floor (sqrt ( fromIntegral n))

main :: IO()
main = putStrLn $ show $ sieveOfEratosthenes2 10001