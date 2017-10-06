{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module VectorSpaces where

import Groups
import Categories

class (AbelianGroup v, Field f) => VectorSpace v f where
    infixl 7 *^
    (*^) :: f -> v -> v
    (*^) a b = b ^* a
    infixl 7 ^*
    (^*) :: v -> f -> v
    (^*) a b = b *^ a

instance (Field f) => VectorSpace f f where
    (*^) a b = a * b


instance (Field f) => AbelianMonoid (f->f) where
    f + g = \x -> f x + g x
    zero = \x -> zero
instance (Field f) => AbelianGroup (f->f) where
    neg f = neg . f
instance (Field f) => Semigroup (f->f) where
    f * g = f . g
instance (Field f) => Monoid (f->f) where
    one = id
instance (Field f) => Ring (f->f)
instance (Field f) => VectorSpace (f->f) f where
    (*^) c f = (*c) . f
