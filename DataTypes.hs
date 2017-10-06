{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module DataTypes (module DataTypes) where

import Categories
import Groups
import Order
import VectorSpaces
import GHC.Base (Eq (..), Bool (..), (&&), error, flip)
import GHC.Show (Show (..))


fst (a, b) = a
snd (a, b) = b

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
    (a,b) * (c,d) = (a*c, b*d)
instance (Monoid a, Monoid b) => Monoid (a, b) where
    one = (one, one)
instance (AbelianMonoid a, AbelianMonoid b) => AbelianMonoid (a, b) where
    (a,b) + (c,d) = (a+c, b+d)
    zero = (zero, zero)
instance (AbelianGroup a, AbelianGroup b) => AbelianGroup (a, b) where
    neg (a, b) = (neg a, neg b)
instance (Ring a, Ring b) => Ring (a, b)
--(a, b) does not form a field because, for example, (1, 0)^-1 does not exist.
instance Functor ((,) b) where
    fmap f (a, b) = (a, f b)
instance Functor ((->) s) where
    fmap f g = f . g

type State s a = Composition ((->) s) ((,) s) a
instance CMonoid NaturalTransformation Functor Composition Id (Composition ((->) s) ((,) s)) where --state monad
    mult = NaturalTransformation (Compose . liftComp flattenState) where
        flattenState :: State s (State s a) -> s -> (s, a)
        flattenState f = \state -> let (st, a) = unCompose f state in unCompose a st
    unit = NaturalTransformation (Compose . makeState . unId) where
        makeState a = \s -> (s, a)
instance Monad (Composition ((->) s) ((,) s))


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


data Vector2 a = V2 !a !a

instance (Eq a) => Eq (Vector2 a) where
    (V2 a b) == (V2 c d) = (a == c) && (b == d)
instance (AbelianMonoid m) => AbelianMonoid (Vector2 m) where
    (V2 a b) + (V2 c d) = V2 (a+c) (b+d)
    zero = V2 zero zero
instance (AbelianGroup g) => AbelianGroup (Vector2 g) where
    neg (V2 a b) = V2 (neg a) (neg b)
instance Functor Vector2 where
    fmap f (V2 a b) = V2 (f a) (f b)


data Vector3 a = V3 !a !a !a
