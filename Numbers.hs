{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Numbers (module Numbers, Integer, Int, Float, Double) where

import GHC.Prim
import Groups
import Order
import Strings
import DataTypes
import GHC.Base (Int (..) , Eq(..), Bool (True), (&&), Ord(..), error, String, (++), undefined, (.), id)
import GHC.Enum
import GHC.Show (Show (..))
import GHC.Float {- (Double (..), Float (..), plusDouble, minusDouble, negateDouble, timesDouble, fabsDouble, divideDouble,
    plusFloat, minusFloat, negateFloat, timesFloat, fabsFloat, divideFloat,
    int2Double, double2Float, float2Double, int2Float) -} hiding (Floating (..), Fractional (..))
import qualified GHC.Real as R (truncate, floor, round, ceiling, Integral(..), fromIntegral, fromRational, Ratio ((:%)))
import qualified GHC.Num as N (Num (fromInteger, negate, (*), (+)))
import GHC.Integer

class (EuclideanDomain i, Numeric i, Enum i) => Integral i where
    toInteger :: i -> Integer

infixr 8 ^
(^) :: (Monoid m, Integral i) => m -> i -> m
(^) x i
    | i == 0 = one
    | True   = let rec = (x*x) ^ (div i 2) in
                  case (mod i 2) of
                    1 -> x * rec
                    _ -> rec

class Numeric a where
    fromIntegral :: (Integral i) => i -> a
    fromFloating :: (Floating f) => f -> a
    toDouble :: a -> Double

class (OrderedField f, Algebraic f) => Floating f where
    floor :: f -> Integer --TODO: Use my Integral?
    round :: f -> Integer
    truncate :: f -> Integer
    ceiling :: f -> Integer
    floating2Double :: f -> Double
    double2Floating :: Double -> f

class (Field f, Numeric f) => Algebraic f where
    pi :: f
    sqrt :: f -> f
    exp :: f -> f
    log :: f -> f
    sin :: f -> f
    cos :: f -> f
    tan :: f -> f
    sinh :: f -> f
    sinh x = (exp x - exp (neg x)) / 2
    cosh :: f -> f
    cosh x = (exp x + exp (neg x)) / 2
    tanh :: f -> f
    tanh x = sinh x / cosh x
    {- TODO: Implement these later
    asin :: f -> f
    acos :: f -> f
    atan :: f -> f
    asinh :: f -> f
    acosh :: f -> f
    atanh :: f -> f
    -}

data Rational = !Integer :/ !Integer --TODO: Make Integer
infixl 7 //
(//) :: Integer -> Integer -> Rational
_ // 0 = error "Division by zero."
a // b = let
    d = gcd a b
    s = signum b
  in (s * div a d) :/ (s * div b d)

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

rational2Floating :: (Numeric f, Field f) => Rational -> f
rational2Floating (a :/ b) = fromInteger a / fromInteger b

--For RebindableSyntax
fromGHCRational :: R.Ratio Integer -> Rational
fromGHCRational (a R.:% b) = a :/ b
fromRational :: (Numeric f, Field f) => R.Ratio Integer -> f
fromRational = rational2Floating . fromGHCRational
fromInteger :: (Numeric a) => Integer -> a
fromInteger = fromIntegral


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
    zero = 0 :: Int
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
    zero = 0
instance AbelianGroup Float where
    neg = negateFloat
    (-) = minusFloat
instance Semigroup Float where
    (*) = timesFloat
instance Monoid Float where
    one = 1
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
--Lifted straight from GHC.Float's Floating implementation.
instance  Algebraic Float  where
    pi                  =  3.141592653589793238
    exp x               =  expFloat x
    log x               =  logFloat x
    sqrt x              =  sqrtFloat x
    sin x               =  sinFloat x
    cos x               =  cosFloat x
    tan x               =  tanFloat x
    sinh x              =  sinhFloat x
    cosh x              =  coshFloat x
    tanh x              =  tanhFloat x

instance AbelianMonoid Double where
    (+) = plusDouble
    zero = 0
instance AbelianGroup Double where
    neg = negateDouble
    (-) = minusDouble
instance Semigroup Double where
    (*) = timesDouble
instance Monoid Double where
    one = 1
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
--Lifted straight from GHC.Float's Floating implementation.
instance Algebraic Double where
    pi                  =  3.141592653589793238
    exp x               =  expDouble x
    log x               =  logDouble x
    sqrt x              =  sqrtDouble x
    sin  x              =  sinDouble x
    cos  x              =  cosDouble x
    tan  x              =  tanDouble x
    sinh x              =  sinhDouble x
    cosh x              =  coshDouble x
    tanh x              =  tanhDouble x

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
    fromFloating x = fromFloating_inner x 1 0 -- Diophantine approximation through continued fraction convergents.
        where
            fromFloating_inner x qn1 qn2
                | qn1 > 8388608 = zero {- 2^23, number of precision bits in a float. -}
                | True = let xFlr = floor x
                             tailApprox = fromFloating_inner (inv (x - fromIntegral xFlr)) (xFlr * qn1 + qn2) qn1
                         in  case tailApprox of
                             0 -> xFlr // 1
                             _ -> inv tailApprox + xFlr // 1
    toDouble = rational2Floating
