-- The Fibonacci sequence is defined by the recurrence relation:

-- Fn = Fn-1 + Fn-2, where F1 = 1 and F2 = 1.
-- Hence the first 12 terms will be:

-- F1 = 1
-- F2 = 1
-- F3 = 2
-- F4 = 3
-- F5 = 5
-- F6 = 8
-- F7 = 13
-- F8 = 21
-- F9 = 34
-- F10 = 55
-- F11 = 89
-- F12 = 144
-- The 12th term, F12, is the first term to contain three digits.

-- What is the first term in the Fibonacci sequence to contain 1000 digits?

fibonacci :: (Integral a) => a -> a
fibonacci 1 = 1
fibonacci 2 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

bigFib :: (Integral a) => a -> a
bigFib digits = bigFibAux 1 1 3 (digits -1)

bigFibAux :: (Integral a) => a -> a -> a -> a -> a
bigFibAux f1 f2 index digits
  | f3 >= (10^digits) = index
  | otherwise = bigFibAux f2 f3 (succ index) digits
  where f3 = f1 + f2
  
main :: IO()
main = putStrLn $ show $ bigFib 1000
  