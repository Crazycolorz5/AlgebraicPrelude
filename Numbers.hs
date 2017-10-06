{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Numbers (module Numbers, Integer, Int, Float, Double) where

import GHC.Prim
import Groups
import Order
import Syntax
import DataTypes.List
import GHC.Base (Int (..) , Eq(..), Bool (True), (&&), Ord(..), error, String, undefined, (.), id)
import GHC.Enum
import GHC.Show (Show (..))
import GHC.Float hiding (Floating (..), Fractional (..))
import qualified GHC.Real as R (truncate, floor, round, ceiling, Integral(..), fromIntegral, fromRational, Ratio ((:%)))
import qualified GHC.Num as N (Num (fromInteger, negate, (*), (+)))
import GHC.Integer

class (EuclideanDomain i, Numeric i, Enum i, Ord i) => Integral i where
    toInteger :: i -> Integer
    quot :: i -> i -> i
    quot a b = let (q,r) = (quotRem a b) in q
    rem :: i -> i -> i
    rem a b = let (q,r) = (quotRem a b) in r
    quotRem :: i -> i -> (i, i)
    quotRem a b = let q = quot a b; r = rem a b in (q, r)

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

nan :: (Floating f) => f
nan = 0/0

pi :: (Numeric f, Field f) => f
pi = 3.141592653589793238

class (Field f, Numeric f) => Algebraic f where
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
    asin :: f -> f
    acos :: f -> f
    atan :: f -> f
    asinh :: f -> f
    asinh x = log (x + sqrt (1+x*x))
    acosh :: f -> f
    acosh x = log (x + (x+1) * sqrt ((x-1)/(x+1)))
    atanh :: f -> f
    atanh x = log ((x+1) / sqrt (1-x*x))

data Rational = !Integer :/ !Integer
infixl 7 //
(//) :: Integer -> Integer -> Rational
_ // 0 = error "Division by zero."
a // b = let
    d = gcd a b
    s = signum b
  in (s * div a d) :/ (s * div b d)

data Complex = Complex Double Double
i = Complex 0 1
instance Semigroup Complex where
    (Complex a b) * (Complex c d) = Complex (a*c - b*d) (b*c + a*d)
instance Monoid Complex where
    one = Complex 1 0
instance Group Complex where
    inv (Complex a b) = let denom = (a*a + b*b) in Complex (a/denom) (neg b/denom)
instance AbelianMonoid Complex where
    (Complex a b) + (Complex c d) = Complex (a+c) (b+d)
    zero = Complex 0 0
instance AbelianGroup Complex where
    neg (Complex a b) = Complex (neg a) (neg b)
instance Show Complex where
    show (Complex a b) = if b >= 0
        then show a * "+" * show b * "i"
        else show a * "-" * show (neg b) * "i"
instance Ring Complex
instance Eq Complex where
    (Complex a b) == (Complex c d) = a == c && b == d
instance Field Complex
instance Numeric Complex where
    fromIntegral x = Complex (fromIntegral x) 0
    fromFloating x = Complex (fromFloating x) 0
    toDouble = modulus
instance Algebraic Complex where
    sqrt z = fromArg (arg z / 2) (sqrt (modulus z))
    exp (Complex a b) = fromArg b (exp a)
    log z = Complex (log (modulus z)) (arg z)
    sin z = (exp(i*z) - exp(neg i*z)) / (2*i)
    cos z = (exp(i*z) + exp(neg i*z)) / 2
    tan z = sin z / cos z
    asin z = neg i * log(i*z + sqrt(1-(z*z)))
    acos z = neg i * log(z + sqrt(z*z-1))
    atan z = log((i-z)/(i+z))/(2*i)


arg :: Complex -> Double
arg (Complex a b) = atan2 b a
{-
    | a > 0 && b >= 0 = atan (b/a)
    | a > 0 && b < 0  = 2*pi + atan (b/a)
    | a < 0 = pi + atan(b/a)
    | a == 0 && b > 0 = pi/2
    | a == 0 && b < 0 = neg pi/2
    | True = nan
-}

modulus :: Complex -> Double
modulus (Complex a b) = sqrt (a*a + b*b)

fromArg :: Double -> Double -> Complex
fromArg arg r = (Complex (r * cos arg) (r * sin arg))

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

rational2Field :: (Numeric f, Field f) => Rational -> f
rational2Field (a :/ b) = fromInteger a / fromInteger b

--For RebindableSyntax
fromGHCRational :: R.Ratio Integer -> Rational
fromGHCRational (a R.:% b) = a :/ b
fromRational :: (Numeric f, Field f) => R.Ratio Integer -> f
fromRational = rational2Field . fromGHCRational
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
    divMod = R.divMod
instance Integral Integer where
    toInteger = id
    quotRem = R.quotRem

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
    divMod = R.divMod
instance Integral Int where
    toInteger (I# i) = smallInteger i
    quotRem = R.quotRem

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
    exp x               =  expFloat x
    log x               =  logFloat x
    sqrt x              =  sqrtFloat x
    sin x               =  sinFloat x
    cos x               =  cosFloat x
    tan x               =  tanFloat x
    sinh x              =  sinhFloat x
    cosh x              =  coshFloat x
    tanh x              =  tanhFloat x
    asin x              =  asinFloat x
    acos x              =  acosFloat x
    atan x              =  atanFloat x

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
    exp x               =  expDouble x
    log x               =  logDouble x
    sqrt x              =  sqrtDouble x
    sin  x              =  sinDouble x
    cos  x              =  cosDouble x
    tan  x              =  tanDouble x
    sinh x              =  sinhDouble x
    cosh x              =  coshDouble x
    tanh x              =  tanhDouble x
    asin  x             =  asinDouble x
    acos  x             =  acosDouble x
    atan  x             =  atanDouble x

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
    toDouble = rational2Field
