module Order where
{-# LANGUAGE NoImplicitPrelude #-}

import Groups
import GHC.Base (Eq (..), Ord (..), Maybe, Bool, not, (&&), otherwise)
import Prelude ()

class (Eq s) => PartialOrder s where
    (-<) :: s -> s -> Maybe Bool
--Satisfying reflexivity and transitivity and antisymmetry

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

class (OrderedRing f, Field f) => OrderedField f


class (OrderedRing d) => EuclideanDomain d where
    quot :: d -> d -> d
    quot a b = let (q,r) = (quotRem a b) in q
    rem :: d -> d -> d
    rem a b = let (q,r) = (quotRem a b) in r
    --p = q * div p q + mod p q satisfying 0 <= mod p q < abs q
    quotRem :: d -> d -> (d, d)
