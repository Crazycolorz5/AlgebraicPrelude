module Groups where
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstrainedClassMethods #-}
import GHC.Base (Monoid (..), Functor (..), error)

class Semigroup s where
    sgmul :: s -> s -> s

class (Monoid g, Eq g) => Group g where
    gmul :: g -> g -> g
    gmul = mappend
    e :: g
    e = mempty
    ginv :: g -> g
    ginv = gdiv e
    gdiv :: g -> g -> g
    gdiv a b = gmul a (ginv b)

class (Eq g) => AddGroup g where
    gadd :: g -> g -> g
    gzero :: g
    gneg :: g -> g
    gsub :: g -> g -> g
    gsub a b = gadd a (gneg b)

data Additive a = Add a deriving Eq
instance Functor Additive where
    fmap f (Add a) = Add (f a)

instance (Group g) => AddGroup (Additive g) where
    gadd (Add a) (Add b) = Add (gmul a b)
    gzero = Add e
    gneg (Add a) = Add (ginv a)

class (AddGroup r, Monoid r) => Ring r where
    (*) :: r -> r -> r
    (*) = mappend
    (+) :: r -> r -> r
    (+) = gadd
    (-) :: r -> r -> r
    (-) = gsub
    zero :: r
    zero = gzero
    one :: r
    one = mempty
    neg :: r -> r
    neg = gneg

class (Ring r) => AbelianRing r

{-
class (AbelianRing d) => EucideanDomain d where
    div :: d -> d -> d
    div a b = fst (divRem a b)
    rem :: d -> d -> d
    rem a b = snd (divRem a b)
    divRem :: d -> d -> (d, d)
    --divRem p q = if euclidean
    euclidean :: d -> Int --TODO: make Integer
-}

class (Ring f, Group f) => Field f where
    (/) :: f -> f -> f
    (/) = gdiv
    inv :: f -> f
    inv x = if x == zero then error "Inverse of additive identity." else ginv x
