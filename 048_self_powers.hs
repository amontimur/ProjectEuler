sumOfExp :: (Integral a) => a -> a
sumOfExp n = sum [x^x | x <- [1..n]]

main :: IO()
main = putStrLn $ show $ sumOfExp 1000