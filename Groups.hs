module Groups where
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstrainedClassMethods #-}
import GHC.Base (Functor (..), error, Eq(..))
import Prelude ()

class Semigroup s where
    sgmul :: s -> s -> s

class Monoid m where
    (*) :: m -> m -> m
    one :: m

class (Monoid g, Eq g) => Group g where
    inv :: g -> g
    inv = (one /)
    (/) :: g -> g -> g
    (/) a b = a * (inv b)

class (Eq g) => AddGroup g where
    (+) :: g -> g -> g
    zero :: g
    neg :: g -> g
    (-) :: g -> g -> g
    (-) a b = a + (neg b)

data Additive a = Add a deriving Eq
instance Functor Additive where
    fmap f (Add a) = Add (f a)

instance (Group g) => AddGroup (Additive g) where
    (+) (Add a) (Add b) = Add (a * b)
    zero = Add one
    neg (Add a) = Add (inv a)

class (AddGroup r, Monoid r) => Ring r


class (Ring f, Group f) => Field f --where
    --inv x = if x == zero then error "Inverse of additive identity." else inv x
