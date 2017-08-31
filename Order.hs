module Order where
{-# LANGUAGE NoImplicitPrelude #-}

import Groups (Field, Ring (..))
import GHC.Base (Eq (..), Ord (..), Maybe, Bool, not, (&&), otherwise)
import Prelude ()

class (Eq s) => PartialOrder s where
    (-<) :: s -> s -> Maybe Bool
--Satisfying reflexivity and transitivity and antisymmetry

{- Is basically just Ord...
class (Eq s) => TotalOrder s where
    (<=) :: s -> s -> Bool
    (>) :: s -> s -> Bool
    (>) a b = b <= a
    (<) :: s -> s -> Bool
    (<) a b = a <= b && a /= b
    (>=) :: s -> s -> Bool
    (>=) a b = b < a
-}
class (Ord f, Ring f) => OrderedRing f where
    isPositive :: f -> Bool
    isPositive = (zero <)
    isNegative :: f -> Bool
    isNegative = (< zero)
    abs :: f -> f
    abs x
        | isPositive x = x
        | otherwise    = neg x
    signum :: f -> f
    signum x
        | isPositive x = one
        | isNegative x = neg one
        | otherwise    = zero
