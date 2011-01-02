module Math.Polynomial where

import Data.List (intercalate)

data Polynomial a =
    Polynomial { coefs :: [a] }
    deriving Eq

poly :: Num a => [a] -> Polynomial a
poly = Polynomial . reverse . dropWhile (== 0) . reverse

instance Show a => Show (Polynomial a) where
    show (Polynomial []) = "{0}"
    show (Polynomial xs) =
        "{" ++ intercalate " + " (reverse (zipWith forEach [(0::Integer)..] xs)) ++ "}"
        where
            forEach idx x = show x ++ " * x^" ++ show idx

zero :: Polynomial a
zero = Polynomial []

x :: Num a => Polynomial a
x = Polynomial [0, 1]

-- Not really demanding that 'a' supports fromInteger
instance Num a => Num (Polynomial a) where
    Polynomial x + Polynomial y =
        poly $ zipWith (+) (pad (length y) x) (pad (length x) y)
        where
            pad count list = list ++ replicate (count - length list) 0

    negate (Polynomial x) = Polynomial (map negate x)

    Polynomial [] * Polynomial _ = Polynomial []
    Polynomial (x:xs) * Polynomial y =
        Polynomial (map (* x) y) + Polynomial (0 : xsy)
        where
            Polynomial xsy = Polynomial xs * Polynomial y

    fromInteger 0 = zero
    fromInteger x = Polynomial [fromInteger x]

    abs p = p * signum p
    signum (Polynomial p) = Polynomial [signum (last p)]

instance Eq a => Ord (Polynomial a)
instance Enum (Polynomial a)
instance Num a => Real (Polynomial a)
instance Integral a => Integral (Polynomial a) where
    divMod (Polynomial []) x = (zero, zero)
    divMod polyX@(Polynomial x) polyY@(Polynomial y)
        | degX < degY = (zero, polyX)
        | otherwise = (highDiv + divLow, highModHighPoly + modLow)
        where
            highDiv = poly $ replicate (degX - degY) 0 ++ [div (last x) (last y)]
            highMod = polyX - highDiv * polyY
            (highModLow, highModHigh) = splitAt degX $ coefs highMod
            highModHighPoly = poly $ replicate degX 0 ++ highModHigh
            (divLow, modLow) = divMod (Polynomial highModLow) polyY
            degX = length x - 1
            degY = length y - 1
    quotRem = divMod
