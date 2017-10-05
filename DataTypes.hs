{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module DataTypes (module DataTypes, Maybe (..)) where

import Categories
import Groups
import Order
import Data.List (foldl') --TODO: Use catamorphisms
import Data.Foldable (Foldable)
import GHC.Base (Eq (..), Maybe (..), Bool (..), (&&), error, flip)
import GHC.Show (Show (..))

instance Functor [] where
    fmap f [] = []
    fmap f (x:xs) = f x:fmap f xs

instance Semigroup [a] where
    (*) a b = case a of
        [] -> b
        (x:xs) -> x : xs * b
instance Monoid [a] where
    one = []
instance (PartialOrder a) => PartialOrder [a] where
    [] -< _ = Just True
    (x:xs) -< [] = Just False
    (x:xs) -< (y:ys) = if x == y then xs -< ys else x -< y
product :: (Foldable t, Monoid a) => t a -> a
product = foldl' (*) one
sum :: (Foldable t, AbelianMonoid a) => t a -> a
sum = foldl' (+) zero

infixr 5 ++
infixr 5 :
(++) :: [a] -> [a] -> [a]
(++) = (*)
map :: (a -> b) -> [a] -> [b]
map = fmap

filter _ [] = []
filter p (x:xs) = if p x then x : filter p xs else filter p xs

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (a:as) = Just (a, as)

head l = case uncons l of
    Just (h, t) -> h
    Nothing -> error "Head of empty list."

tail l = case uncons l of
    Just (h, t) -> t
    Nothing -> error "Tail of empty list."

reverse :: [a] -> [a]
reverse = foldl' (flip (:)) []

takeWhile p [] = []
takeWhile p (x:xs) = if p x then x : takeWhile p xs else []

instance CMonoid NaturalTransformation Functor Composition Id [] where
    mult = NaturalTransformation (liftComp product)
    unit = NaturalTransformation ((: one) . unId)

instance Monad []


--data Maybe a = Just a | Nothing deriving (Eq, Show)
instance Functor Maybe where
    fmap f Nothing = Nothing
    fmap f (Just a) = Just (f a)
instance Semigroup s => Semigroup (Maybe s) where
    Nothing * _ = Nothing
    _ * Nothing = Nothing
    (Just a) * (Just b) = Just (a * b)
instance (Monoid m) => Monoid (Maybe m) where
    one = Just one
instance (AbelianMonoid m) => AbelianMonoid (Maybe m) where
    Nothing + x = x
    x + Nothing = x
    (Just a) + (Just b) = Just (a + b)
    zero = Nothing
instance (AbelianGroup g) => AbelianGroup (Maybe g) where
    neg (Just x) = Just (neg x)
    neg Nothing = Nothing
instance (Ring r) => Ring (Maybe r)
instance CMonoid NaturalTransformation Functor Composition Id Maybe where
    mult = NaturalTransformation (liftComp flattenMaybe) where
        flattenMaybe Nothing = Nothing
        flattenMaybe (Just x) = x
    unit = NaturalTransformation (Just . unId)
instance Monad Maybe
{-
GHCi> do a <- [1,2,3]; [a*2]
[2,4,6]
-}

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
