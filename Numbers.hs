{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Numbers (Numeric(..), Floating(..), Integral(..), Rational, (//), Int, Integer, Float, Double, rational2Floating) where

import GHC.Prim
import Groups
import Order
import Strings
import DataTypes
import GHC.Base (Int (..) , Eq(..), (&&), Ord(..), error, String, (++), undefined, (.), id)
import GHC.Show (Show (..))
import GHC.Float (Double (..), Float (..), plusDouble, minusDouble, negateDouble, timesDouble, fabsDouble, divideDouble,
    plusFloat, minusFloat, negateFloat, timesFloat, fabsFloat, divideFloat,
    int2Double, double2Float, float2Double, int2Float)
import GHC.Real (gcd)
import qualified GHC.Real as R (truncate, floor, round, ceiling, Integral(..), fromIntegral)
import qualified GHC.Num as N (Num (fromInteger, negate, (*), (+)))
import GHC.Integer
import Prelude ()

class (EuclideanDomain i) => Integral i where
    toInteger :: i -> Integer

class Numeric a where
    fromIntegral :: (Integral i) => i -> a
    fromFloating :: (Floating f) => f -> a
    toDouble :: a -> Double

class (OrderedField f, Numeric f) => Floating f where
    floor :: f -> Integer --TODO: Use my Integral?
    round :: f -> Integer
    truncate :: f -> Integer
    ceiling :: f -> Integer
    floating2Double :: f -> Double
    double2Floating :: Double -> f

instance AbelianMonoid Integer where --TODO: Maybe not dependent on GHC's Num?
    (+) = (N.+)
    zero = 0
instance AbelianGroup Integer where --TODO: Same
    neg = N.negate
instance Semigroup Integer where
    (*) = (N.*)
instance Monoid Integer where
    one = 1
instance Ring Integer
instance OrderedRing Integer
instance EuclideanDomain Integer where
    quotRem = R.quotRem
instance Integral Integer where
    toInteger = id

instance AbelianMonoid Int where
    (+) (I# x) (I# y) = I# (x +# y)
    zero = 0
instance AbelianGroup Int where
    neg (I# x) = I# (negateInt# x)
instance Semigroup Int where
    (*) (I# x) (I# y) = I# (x *# y)
instance Monoid Int where
    one = 1
instance Ring Int
instance OrderedRing Int
instance EuclideanDomain Int where
    quotRem = R.quotRem
instance Integral Int where
    toInteger (I# i) = smallInteger i

instance AbelianMonoid Float where
    (+) = plusFloat
    zero = 0.0
instance AbelianGroup Float where
    neg = negateFloat
    (-) = minusFloat
instance Semigroup Float where
    (*) = timesFloat
instance Monoid Float where
    one = 1.0
instance Group Float where
    (/) = divideFloat
instance Ring Float
instance Field Float
instance OrderedRing Float where
    abs = fabsFloat
instance OrderedField Float
instance Floating Float where
    floor = R.floor
    round = R.round
    truncate = R.truncate
    ceiling = R.ceiling
    double2Floating = double2Float
    floating2Double = float2Double

instance AbelianMonoid Double where
    (+) = plusDouble
    zero = 0.0
instance AbelianGroup Double where
    neg = negateDouble
    (-) = minusDouble
instance Semigroup Double where
    (*) = timesDouble
instance Monoid Double where
    one = 1.0
instance Group Double where
    (/) = divideDouble
instance Ring Double
instance Field Double
instance OrderedRing Double where
    abs = fabsDouble
instance OrderedField Double
instance Floating Double where
    floor = R.floor
    round = R.round
    truncate = R.truncate
    ceiling = R.ceiling
    double2Floating = id
    floating2Double = id

data Rational = !Int :/ !Int --TODO: Make Integer
(//) :: Int -> Int -> Rational
_ // 0 = error "Division by zero."
a // b = let
    d = gcd a b
    s = signum b
  in (s * R.div a d) :/ (s * R.div b d)

instance Show Rational where
    show (a :/ b) = show a * " / " * show b

instance Semigroup Rational where
    (*) (a :/ b) (c :/ d) = (a*c) // (b*d)
instance Monoid Rational where
    one = 1 :/ 1
instance Eq Rational where
    (a :/ b) == (c :/ d) = (a == c) && (b == d)
instance Group Rational where
    (a :/ b) / (c :/ d) = (a*d) // (b*c)
instance AbelianMonoid Rational where
    (+) (a :/ b) (c :/ d) = (a*d + b*c) // (b*d)
    zero = 0 :/ 1
instance AbelianGroup Rational where
    neg (a :/ b) = neg a // b
instance Ring Rational
instance Field Rational
instance Ord Rational where
    (a :/ b) <= (c :/ d) = (a*d) <= (c*b)
instance OrderedRing Rational
instance OrderedField Rational

rational2Floating :: (Floating f) => Rational -> f
rational2Floating (a :/ b) = fromIntegral a / fromIntegral b

-- TODO: Reduce dependence on GHC's Num?
instance Numeric Int where
    fromIntegral = N.fromInteger . toInteger
    fromFloating = N.fromInteger . truncate
    toDouble = int2Double

instance Numeric Integer where
    fromIntegral = toInteger
    fromFloating = truncate
    toDouble x = D# (doubleFromInteger x)

instance Numeric Float where
    fromIntegral = N.fromInteger . toInteger
    fromFloating = double2Floating . floating2Double
    toDouble = floating2Double

instance Numeric Double where
    fromIntegral = N.fromInteger . toInteger
    fromFloating = double2Floating . floating2Double
    toDouble = id

instance Numeric Rational where
    fromIntegral x = fromIntegral x // 1
    fromFloating = undefined --Rational approximations TODO
    toDouble = rational2Floating
