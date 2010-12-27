module Math.Modular where

data NumberMod a = NumberMod
    { remainder :: a
    , modulus :: a
    } deriving (Eq, Show)

(%) :: Integral a => a -> a -> NumberMod a
x % m = NumberMod (mod x m) m

inverse :: Integral a => NumberMod a -> NumberMod a
inverse (NumberMod x m) =
    if g == 1 then r % m else NumberMod 0 0
    where
        (g, r) = gcdComb (x, 1) (m, 0)
        gcdComb (0, _) r = r
        gcdComb xx@(x, xc) (y, yc) =
            gcdComb (m, (yc - d * xc)) xx
            where
                (d, m) = divMod y x

instance Integral a => Num (NumberMod a) where
    NumberMod x0 m0 + NumberMod x1 m1 = (x0 + x1) % gcd m0 m1
    NumberMod x0 m0 * NumberMod x1 m1 = (x0 * x1) % gcd m0 m1
    -- Functions that we must only implement because Num kinda sucks
    abs = id
    fromInteger = const $ NumberMod 0 1
    signum = const 1

instance Integral a => Fractional (NumberMod a) where
    dividend / divisor = dividend * inverse divisor
    -- Functions that we must only implement because Fractional kinda sucks
    fromRational = const $ NumberMod 0 1
