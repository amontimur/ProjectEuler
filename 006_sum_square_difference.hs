sumOfSquares :: (Integral a) => a -> a
sumOfSquares n = sum [x^2 | x <- [1..n]]

sqareOfSums :: (Integral a) => a -> a
sqareOfSums n = (sum [1..n])^2

sumSquareDifference :: (Integral a) => a -> a
sumSquareDifference n = (sqareOfSums n) - (sumOfSquares n)

main :: IO()
main = putStrLn $ show $ sumSquareDifference 100