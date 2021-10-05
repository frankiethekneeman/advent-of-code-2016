module CRT (
    CongruenceClass(..),
    combine,
    combineAll,
    reduce,
) where

import Control.Monad(foldM)

data CongruenceClass = CongruenceClass {remainder, modulus :: Integer} deriving (Show)

combine :: CongruenceClass -> CongruenceClass -> Maybe CongruenceClass
combine (CongruenceClass r1 m1) (CongruenceClass r2 m2)
    | g == 1 = Just $ reduce $ CongruenceClass ((b * m2 * r1) + (a * m1 * r2)) (m1 * m2)
    | otherwise = Nothing
    where (g, a, b) = extendedGCD m1 m2

combineAll :: [CongruenceClass] -> Maybe CongruenceClass
combineAll [] = Nothing
combineAll (x:xs) = foldM combine x xs

extendedGCD :: Integer -> Integer -> (Integer, Integer, Integer)
extendedGCD m n
    | m >= n = extendedEuclidian m n 1 0 0 1
    | otherwise = (gcd, s, t)
        where (gcd, t, s) = extendedGCD n m

reduce :: CongruenceClass -> CongruenceClass
reduce (CongruenceClass r n) = CongruenceClass (r `mod` n) n

extendedEuclidian :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> (Integer, Integer, Integer)
extendedEuclidian m n s s' t t'
    | r == 0 = (n, s', t')
    | otherwise = extendedEuclidian n r s' s'' t' t''
    where (q, r) = divMod m n
          s'' = s - q * s'
          t'' = t - q * t'
