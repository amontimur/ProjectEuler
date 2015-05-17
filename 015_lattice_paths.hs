-- Starting in the top left corner of a 2×2 grid, and only being able to move 
-- to the right and down, there are exactly 6 routes to the bottom right corner.
-- 
-- How many such routes are there through a 20×20 grid?

-- returns the number of lattice paths according to the question

import Data.Bits


latticePaths :: (Integral a) => a -> a
latticePaths n = binomial (n*2) n

-- returns the factorial of n
factorial :: (Integral a) => a -> a
factorial n = product [1..n]

-- returns the binomial coefficient n over k
binomial :: (Integral a) => a -> a -> a
binomial n k = div (factorial n) (factorial k * factorial (n - k))

main :: IO()
main = putStrLn $ show $ latticePaths 20

-- unused:

-- multiplies n by 2^x
mul2n :: Int -> Int -> Int
mul2n n x = shiftL n x
