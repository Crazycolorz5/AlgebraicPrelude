{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DataTypes where

import Categories
import Groups (Monoid (..))
import Data.List (foldl') --TODO: Use catamorphisms

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

instance CatMonoid NaturalTransformation Functor Composition Id [] where
    mult = NaturalTransformation (liftComp product)
    unit = NaturalTransformation ((: one) . unId)

instance Monad []

{-
GHCi> do a <- [1,2,3]; [a*2]
[2,4,6]
-}
