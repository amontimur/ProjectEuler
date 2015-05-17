module AKS where

import           Data.Bits
import           Data.Maybe
import           Math.Polynomial
import           Math.Polynomial.NumInstance

main :: IO ()
main = putStrLn "Hello World!"

aks :: Integer -> Bool
aks n
  | isNothing . perfectPower $ n = False
  | n `rem` r == 0 = case r `compare` n of
                      EQ -> True
                      LT -> False
                      _  -> error "r should be less than n"
  -- | otherwise =
  where len :: Integer
        len = (+1) . floor . logBase 2 . fromIntegral $ n
        checkR :: Integer -> Bool
        checkR r = (r `rem` n) == 0 || (or [ (n ^ i) `mod` r /= 1 | i <- [1..len^2] ])
        r :: Integer
        r = head [ r | r <- [1..], checkR r ]
        -- prop10 ::

perfectPower :: Integer -> Maybe (Integer,Integer)
perfectPower n = perfectPower' n 2 (floor $ logBase 2 (fromInteger n)) 2 n
    where perfectPower' :: Integer -> Integer -> Integer -> Integer -> Integer -> Maybe (Integer,Integer)
          perfectPower' n e maxE m1 m2
            | e > maxE    = Nothing
            | m1 > m2     = perfectPower' n (e+1) maxE 2 n
            | mToE == n   = Just (m,e)
            | mToE >  n   = perfectPower' n e maxE m1 (m-1)
            | otherwise   = perfectPower' n e maxE (m+1) m2
            where m = div (m1 + m2) 2 -- div truncates towards neg inf
                  mToE = fastExp m e

fastExp :: Integer -> Integer -> Integer
fastExp 0 _ = 0
fastExp a 1 = a
fastExp a n = fastExp' a n 1
  where fastExp' :: Integer -> Integer -> Integer -> Integer
        fastExp' b n y
          | n == 0        = y
          | n .&. 1 == 1  = fastExp' (b*b) (shiftR n 1) (y*b)
          | otherwise     = fastExp' (b*b) (shiftR n 1) y

-- Demo
aPol :: Poly Double
aPol = powPoly (x + constPoly 2) 31

bPol :: Poly Double
bPol = x^29 - one

cPol :: Poly Double
cPol = remPoly aPol bPol

dPol :: Poly Integer
dPol = fmap ( (\x -> rem x 31). floor) cPol

ePol :: Poly Double
ePol = remPoly (fmap fromIntegral dPol) bPol

fPol :: Poly Double
fPol = dPol' - ePol
  where dPol' :: Poly Double
        dPol' =(fmap fromIntegral dPol)
