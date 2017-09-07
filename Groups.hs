{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstrainedClassMethods #-}

module Groups (Monoid (..), AbelianMonoid (..), Group (..), AbelianGroup (..), Ring, Field) where

import GHC.Base (error, Eq(..))

class Monoid m where
    (*) :: m -> m -> m
    one :: m

class AbelianMonoid m where
    (+) :: m -> m -> m
    zero :: m

class (Monoid g, Eq g) => Group g where
    inv :: g -> g
    inv = (one /)
    (/) :: g -> g -> g
    (/) a b = a * (inv b)

class (AbelianMonoid g, Eq g) => AbelianGroup g where
    neg :: g -> g
    neg = (-) zero
    (-) :: g -> g -> g
    (-) a b = a + (neg b)

class (AbelianGroup r, Monoid r) => Ring r

class (Ring f, Group f) => Field f --where
    --Though it's not labeled as abelian, the group joined with the ring is abelian.
    --inv x = if x == zero then error "Inverse of additive identity." else inv x
