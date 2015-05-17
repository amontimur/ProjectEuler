-- n! means n × (n − 1) × ... × 3 × 2 × 1
-- 
-- For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
-- and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
-- 
-- Find the sum of the digits in the number 100!

-- returns the factorial of n
factorial :: (Integral a) => a -> a
factorial n = product [1..n]

-- calculates the digit sum
digitSum :: (Integral a) => a -> a
digitSum n = digitSum' n 0

digitSum' :: (Integral a) => a -> a -> a
digitSum' n dSum
  | n < 10 = dSum + n
  | otherwise = digitSum' (div n 10) (dSum + (mod n 10))
  
main :: IO()
main = putStrLn $ show $ digitSum $ factorial 100