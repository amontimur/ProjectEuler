oddSeq :: (Integral a) => [(a,a,a)]
-- only from siev of erat
oddSeq = [(x,y,z) | x <- primes, y <- primes, z <- primes, x < y, x - y == y - z, checkPermutation x y, checkPermutation y z, y /= z, x /= z]
  where primes = filter (\x -> x > 999) (sieveOfEratosthenes 10000)

checkPermutation :: (Integral a) => a -> a -> Bool
checkPermutation x y = (digitSum x) == (digitSum y ) && (compareDigits x y)

-- returns true if x consits of the same digits (in different order) as y
compareDigits :: (Integral a) => a -> a -> Bool
compareDigits x y 
  | x == y = True
  | elem lastX (digitsToList y) = compareDigits (div x 10) (listToDigits (del lastX (digitsToList y)))
  | otherwise = False
  where lastX = mod x 10

-- sieve of Eratosethenes from 0 to n
sieveOfEratosthenes :: (Integral a) => a -> [a]
sieveOfEratosthenes maxN = sieveOfEratosthenes' (sqrt' maxN) [2..maxN] []

sieveOfEratosthenes' :: (Integral a) => a -> [a] -> [a] -> [a]
sieveOfEratosthenes' endN list primes
  | head list > endN = primes ++ list
  | otherwise = sieveOfEratosthenes' endN (deleteMultiples (head list) list) (head list : primes)

deleteMultiples ::  (Integral a) => a -> [a] -> [a]
deleteMultiples n list = [x | x <- list, (mod x n) /= 0]

sqrt' :: (Integral a) => a -> a
sqrt' n = floor (sqrt ( fromIntegral n))


-- deletes the first instance of n from a list
del :: (Integral a) => a -> [a] -> [a]
del n l = del' n l [] 

del' :: (Integral a) => a -> [a] -> [a] -> [a]
del' n (x : l) h
  | n == x = h ++ l
  | otherwise = del' n l (h ++ [x])

  
-- converts a list to a Integral
listToDigits :: (Integral a) => [a] -> a
listToDigits l
  | l == [] = 0
  | otherwise = listToDigits' 0 l

listToDigits' :: (Integral a) => a -> [a] -> a
listToDigits' n (x : l)
  | l == [] = n*10 + x
  | otherwise = listToDigits' (n*10 + x ) l

  
-- converts an Integral to a list
digitsToList :: (Integral a) => a -> [a]
digitsToList n = digitsToList' n []

digitsToList' :: (Integral a) => a -> [a] -> [a]
digitsToList' n nList
  | n < 10 = n : nList
  | otherwise = digitsToList' (div n 10) ((mod n 10) : nList)

  
-- calculates the digit sum
digitSum :: (Integral a) => a -> a
digitSum n = digitSum' n 0

digitSum' :: (Integral a) => a -> a -> a
digitSum' n dSum
  | n < 10 = dSum + n
  | otherwise = digitSum' (div n 10) (dSum + (mod n 10))
  
main :: IO()
main = putStrLn $ show $ oddSeq