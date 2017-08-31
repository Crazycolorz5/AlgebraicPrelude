module Numbers (Rational, (//), Int, Double) where

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
import GHC.Prim
import Groups
import Order
import Strings
import GHC.Base (Int (..) , Eq(..), (&&), Ord(..), error, String, (++))
import GHC.Show (Show (..))
import GHC.Float (Double (..), plusDouble, minusDouble, negateDouble, timesDouble, fabsDouble, divideDouble)
import GHC.Real (gcd, Integral (div, mod))
import Prelude ()

instance AddGroup Int where
    (+) (I# x) (I# y) = I# (x +# y)
    zero = 0
    neg (I# x) = I# (negateInt# x)
instance Monoid Int where
    (*) (I# x) (I# y) = I# (x *# y)
    one = 1
instance Ring Int
instance OrderedRing Int

instance AddGroup Double where
    (+) = plusDouble
    zero = 0.0
    neg = negateDouble
    (-) = minusDouble
instance Monoid Double where
    (*) = timesDouble
    one = 1.0
instance Group Double where
    (/) = divideDouble
instance Ring Double
instance Field Double
instance OrderedRing Double

data Rational = !Int :/ !Int --TODO: Make Integer
(//) :: Int -> Int -> Rational
_ // 0 = error "Division by zero."
a // b = let
    d = gcd a b
    s = signum b
  in (s * div a d) :/ (s * div b d)

instance Show Rational where
    show (a :/ b) = show a ++ " / " ++ show b

instance Monoid Rational where
    (*) (a :/ b) (c :/ d) = (a*c) // (b*d)
    one = 1 :/ 1
instance Eq Rational where
    (a :/ b) == (c :/ d) = (a == c) && (b == d)
instance Group Rational where
    (a :/ b) / (c :/ d) = (a*d) // (b*c)
instance AddGroup Rational where
    (+) (a :/ b) (c :/ d) = (a*d + b*c) // (b*d)
    zero = 0 :/ 1
    neg (a :/ b) = neg a // b
instance Ring Rational
instance Field Rational
instance Ord Rational where
    (a :/ b) <= (c :/ d) = (a*d) <= (c*b)
instance OrderedRing Rational
