module Numbers (Rational, (//), Int, Double) where

{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples #-}
import GHC.Prim
import Groups
import Order
import Strings
import GHC.Base (Monoid (..), Int (..) , Eq(..), (&&), Ord(..), error, String)
import GHC.Show
import GHC.Float (Double (..), plusDouble, minusDouble, negateDouble, timesDouble, fabsDouble, divideDouble)
import GHC.Real (gcd, Integral (div, mod))
import Prelude ()

instance AddGroup Int where
    gadd (I# x) (I# y) = I# (x +# y)
    gzero = 0
    gneg (I# x) = I# (negateInt# x)
instance Monoid Int where
    mappend (I# x) (I# y) = I# (x *# y)
    mempty = 1
instance Ring Int
instance OrderedRing Int

instance AddGroup Double where
    gadd = plusDouble
    gzero = 0.0
    gneg = negateDouble
    gsub = minusDouble
instance Monoid Double where
    mappend = timesDouble
    mempty = 1.0
instance Group Double where
    gdiv = divideDouble
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
    show (a :/ b) = show a `mappend` " / " `mappend` show b

instance Monoid Rational where
    mappend (a :/ b) (c :/ d) = (a*c) // (b*d)
    mempty = 1 :/ 1
instance Eq Rational where
    (a :/ b) == (c :/ d) = (a == c) && (b == d)
instance Group Rational where
    gdiv (a :/ b) (c :/ d) = (a*d) // (b*c)
instance AddGroup Rational where
    gadd (a :/ b) (c :/ d) = (a*d + b*c) // (b*d)
    gzero = 0 :/ 1
    gneg (a :/ b) = neg a // b
instance Ring Rational
instance Field Rational
instance Ord Rational where
    (a :/ b) <= (c :/ d) = (a*d) <= (c*b)
instance OrderedRing Rational
