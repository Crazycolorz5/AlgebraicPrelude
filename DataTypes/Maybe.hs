{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DataTypes.Maybe (module DataTypes.Maybe, Maybe (..)) where

import GHC.Base (Eq (..), Maybe (..), Bool (..), (&&), error, flip)
import Categories
import Groups

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
