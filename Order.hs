module Order where
{-# LANGUAGE NoImplicitPrelude #-}

import Groups
import GHC.Base (Eq (..), Ord (..), Maybe, Bool, not, (&&), otherwise)
import Prelude ()

class (Eq s) => PartialOrder s where
    infix 4 -<
    (-<) :: s -> s -> Maybe Bool
--Satisfying reflexivity and transitivity and antisymmetry

--TODO: Make it not dependent on Ord
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
    div :: d -> d -> d
    div a b = let (q,r) = (divMod a b) in q
    mod :: d -> d -> d
    mod a b = let (q,r) = (divMod a b) in r
    --p = q * div p q + mod p q satisfying 0 <= mod p q < abs q
    divMod :: d -> d -> (d, d)
    divMod n d = if signum r == neg (signum d) then (q-one, r+d) else qr
                           where qr@(q,r) = quotRem n d --Copied from GHC.Real
    quot :: d -> d -> d
    quot a b = let (q,r) = (quotRem a b) in q
    rem :: d -> d -> d
    rem a b = let (q,r) = (quotRem a b) in r
    quotRem :: d -> d -> (d, d)
