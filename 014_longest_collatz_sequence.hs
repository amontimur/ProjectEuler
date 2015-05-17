import Data.Bits

collatzStep :: Int -> Int
collatzStep n
  | even n = shiftR n 1
  | otherwise = 3*n + 1
  
collatzChain :: Int -> [Int]
collatzChain n = collatzChain' n []
collatzChain' :: Int -> [Int] -> [Int]
collatzChain' n list
  | n == 1 = 1 : list
  | otherwise = collatzChain' (collatzStep n) (n : list)
  
collatzLength :: Int -> Int
collatzLength n = collatzLength' n 0

collatzLength' :: Int -> Int -> Int
collatzLength' n l
  | n == 1 = l + 1
  | otherwise = collatzLength' (collatzStep n) (l+1)
  
-- IntervallStart -> IntervallEnd -> (MaxN, MaxLength)
collatzMaxLength :: Int -> Int -> (Int, Int)
collatzMaxLength a b = collatzMaxLength' a b (0, 0)

-- Intervall -> (N, Length) -> (MaxN, MaxLength)

collatzMaxLength' :: Int -> Int -> (Int,Int) -> (Int,Int)
collatzMaxLength' i j maxl
  | i == j = maxTouple (i, newLength) maxl
  | otherwise = collatzMaxLength' (i+1) j (maxTouple (i, newLength) maxl)
  where newLength = collatzLength' i 0

-- Min -> Max -> (tmpMaxN, tmpMaxLenght) -> (MaxN, MaxLength)
collatzMaxLength'' :: Int -> Int -> (Int, Int)
collatzMaxLength'' i j = foldr maxTouple (0, 0) (zip [i..j] (map collatzLength [i..j]))


-- compares two touples and returns the one with the bigger second entry
maxTouple :: (Ord b) => (a, b) -> (a, b) -> (a, b)
maxTouple t1 t2
  | snd t1 > snd t2 = t1
  | otherwise = t2

main::IO()
main = putStrLn $ show (collatzMaxLength'' 1 1000000)