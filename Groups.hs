{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstrainedClassMethods #-}

module Groups (module Groups) where

import GHC.Base (error, Eq(..), Bool (..))

class Semigroup s where
    infixl 7 *
    (*) :: s -> s -> s

class (Semigroup m) => Monoid m where
    one :: m

class AbelianMonoid m where
    infixl 6 +
    (+) :: m -> m -> m
    zero :: m

class (Monoid g) => Group g where
    inv :: g -> g
    inv = (one /)
    infixl 7 /
    (/) :: g -> g -> g
    (/) a b = a * (inv b)

class (AbelianMonoid g) => AbelianGroup g where
    neg :: g -> g
    neg = (-) zero
    infixl 6 -
    (-) :: g -> g -> g
    (-) a b = a + (neg b)

negate :: (AbelianGroup g) => g -> g
negate = neg

class (AbelianGroup r, Monoid r) => Ring r

class (Ring f, Group f, Eq f) => Field f --where
    --Though it's not labeled as abelian, the group joined with the ring is abelian.
    --inv x = if x == zero then error "Inverse of additive identity." else inv x
