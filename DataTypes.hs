{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DataTypes where

import Categories
import Groups
import Data.List (foldl') --TODO: Use catamorphisms
import GHC.Base (Eq (..))

instance Functor [] where
    fmap f [] = []
    fmap f (x:xs) = f x:fmap f xs

instance Monoid [a] where
    (*) a b = case a of
        [] -> b
        (x:xs) -> x : xs * b
    one = []

product :: (Monoid a) => [a] -> a
product = foldl' (*) one

instance CMonoid NaturalTransformation Functor Composition Id [] where
    mult = NaturalTransformation (liftComp product)
    unit = NaturalTransformation ((: one) . unId)

instance Monad []


data Maybe a = Just a | Nothing deriving Eq
instance Functor Maybe where
    fmap f Nothing = Nothing
    fmap f (Just a) = Just (f a)
instance (Monoid m) => Monoid (Maybe m) where
    Nothing * _ = Nothing
    _ * Nothing = Nothing
    (Just a) * (Just b) = Just (a * b)
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
