-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
-- 
-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

main :: IO()
main = putStrLn $ show $ productOfToupleList $ maxOccuringPrimeFactors $ occuringPrimeFactors [1..20]

productOfToupleList :: (Integral a) => [(a,a)] -> a
productOfToupleList l = foldr (\x y -> touplePower x * y) 1 l

touplePower :: (Integral a) => (a,a) -> a
touplePower (x,y) = x^y

maxOccuringPrimeFactors :: (Integral a) => [(a,a)] -> [(a,a)]
maxOccuringPrimeFactors l = maxOccuringPrimeFactorsAux l []

maxOccuringPrimeFactorsAux :: (Integral a) => [(a,a)]-> [(a,a)]-> [(a,a)]
maxOccuringPrimeFactorsAux l retL
  | l == [] = retL
  | otherwise = maxOccuringPrimeFactorsAux otherList (maxSamesList : retL)
  where otherList = (filter (\x -> fst x /= fst (head l)) l)
        samesList = (filter (\x -> fst x == fst (head l)) l)
        maxSamesList = foldr maxTouple (0,0) samesList

occuringPrimeFactors :: (Integral a) => [a] -> [(a,a)]
occuringPrimeFactors l = foldr (\x y -> x ++ y) [] listOfLists
  where listOfLists = map squeeze $ primefactorsList l

-- lists all prime factors of n
primefactors :: (Integral a) => a -> [a]
primefactors n = primefactorsAux n 2 []

primefactorsAux :: (Integral a) => a -> a -> [a] -> [a]
primefactorsAux n divisor factors
  | n == 1 = factors
  | (mod n divisor) == 0 = primefactorsAux (div n divisor) (divisor) (divisor:factors)
  | otherwise = primefactorsAux n (divisor+1) factors

-- returns a list of lists of prime factors of all numbers in l
primefactorsList :: (Integral a) => [a] -> [[a]]
primefactorsList l = [primefactors x | x <- l]

-- squeezes consecutive instances of an integral to a touple containing 
-- the integral and number of occurences
squeeze :: (Integral a) => [a] -> [(a,a)]
squeeze l = squeezeAux l []

squeezeAux :: (Integral a) => [a] -> [(a,a)] -> [(a,a)]
squeezeAux [] retL = retL
squeezeAux (h:l) retL
  | elem' h retL = squeezeAux l (incToupleList h retL)
  | otherwise = squeezeAux l ((h,1) : retL)

-- increments the second touple element of a given first touple element
incToupleList :: (Integral a) => a -> [(a,a)] -> [(a,a)]
incToupleList n l = map (\x -> if fst x == n  then (n,succ $ snd x) else ( fst x, snd x)) l

-- compares two touples and returns the one with the bigger second entry
maxTouple :: (Ord b) => (a, b) -> (a, b) -> (a, b)
maxTouple t1 t2
  | snd t1 > snd t2 = t1
  | otherwise = t2
  
  
-- checks weather n is in any first position of any touple of a list
elem' :: (Integral a) => a -> [(a,a)] -> Bool
elem' n l = any (\x -> fst x == n) l

elem'Aux :: (Integral a) => a -> [(a,a)] -> Bool
elem'Aux n l
  | l == [] = False
  | n == (fst (head l)) = True
  | otherwise = elem' n (tail l)