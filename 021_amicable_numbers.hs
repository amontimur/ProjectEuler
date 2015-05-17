-- Let d(n) be defined as the sum of proper divisors of n (numbers less than n which 
-- divide evenly into n).
-- If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each 
-- of a and b are called amicable numbers.
-- 
-- For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; 
-- therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; 
-- so d(284) = 220.
-- 
-- Evaluate the sum of all the amicable numbers under 10000.

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

-- returns sum of proper divisors of n 
d :: (Integral a) => a -> a
d 1 = 1
d n = sum $ listProperDivisors n


-- lists all amicable numbers from 1 to n
listAmicableNumbers :: (Integral a) => a -> [(a,a)]
listAmicableNumbers n = [(x,y) | x <- [1..n], y <- [x+1..n], isAmicableNumber x y ]
  where isAmicableNumber a b = d a == b && d b == a
        
sumToupleList :: (Integral a) => [(a,a)] -> a
sumToupleList list = sum $ map (\x -> fst x + snd x) list
        
main :: IO()
main = print $ sumToupleList $ listAmicableNumbers 10000


-- unused:

-- lists all proper Divisors of n sorted
listProperDivisors' :: (Integral a) => a -> [a]
listProperDivisors' 1 = [1]
listProperDivisors' n = quicksort $ 1 : (filter (\x -> x /= n) $ delDuplicates $ map product (subsets $ primefactors n))


-- sorts a list of list based on the length of the sublists
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted 
    
