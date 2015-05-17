-- 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
-- 
-- What is the sum of the digits of the number 2^1000?

-- converts an Integral to a list
digitsToList :: (Integral a) => a -> [a]
digitsToList n = digitsToList' n []

digitsToList' :: (Integral a) => a -> [a] -> [a]
digitsToList' n nList
  | n < 10 = n : nList
  | otherwise = digitsToList' (div n 10) ((mod n 10) : nList)
  
main :: IO()
main = putStrLn $ show $ sum $ digitsToList $ 2^1000