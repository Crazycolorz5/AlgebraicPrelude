{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DataTypes.List where

import Categories
import Groups
import Order
import Syntax
import DataTypes.Maybe
import Data.List (foldl') --TODO: Use catamorphisms
import Data.Foldable (Foldable)
import GHC.Base (Eq (..), Bool (..), (&&), error, flip)

instance Semigroup [a] where
    (*) a b = case a of
        [] -> b
        (x:xs) -> x : xs * b
instance Monoid [a] where
    one = []

instance Functor [] where
    fmap f [] = []
    fmap f (x:xs) = f x:fmap f xs
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

zip = let pair a b = (a, b) in zipWith pair

zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith op (a:as) (b:bs) = op a b : zipWith op as bs

zipWithId z op [] [] = []
zipWithId z op (a:as) [] = op a z : zipWithId z op as []
zipWithId z op [] (b:bs) = op z b : zipWithId z op [] bs
zipWithId z op (a:as) (b:bs) = op a b : zipWithId z op as bs

foldl _ z [] = z
foldl f z (x:xs) = foldl f (f z x) xs

foldl1 f (x:xs) = foldl f x xs

null [] = True
null _ = False

dropWhile p [] = []
dropWhile p (x:xs) = if p x then dropWhile p xs else (x:xs)

length [] = 0
length (x:xs) = 1 + (length xs)

drop n [] = []
drop n (x:xs) = if n == 0 then (x:xs) else drop (n-1) xs

last (x:[]) = x
last (x:xs) = last xs

iterate f seed = seed : fmap f (iterate f seed)

instance CMonoid NaturalTransformation Functor Composition Id [] where
    mult = NaturalTransformation (liftComp product)
    unit = NaturalTransformation ((: one) . unId)

instance Monad []
