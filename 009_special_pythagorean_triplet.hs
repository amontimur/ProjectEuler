specPythagoreanTriplet :: (Integral a) => [(a,a,a)]
specPythagoreanTriplet = [ (a,b,c) | a <- [1..998], b <- [(a+1)..998], c <- [(a+1)..998], (a+b+c)==1000, a^2 + b^2 == c^2 ]

tripleProduct:: (Integral a) => [(a,a,a)] -> a
tripleProduct ((a,b,c) : []) = a*b*c

main :: IO()
main = putStrLn $ show $ tripleProduct specPythagoreanTriplet