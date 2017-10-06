{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Polynomials (module Polynomials) where

import Groups
import Order
import DataTypes
import Categories
import Numbers
import VectorSpaces
import GHC.Base (Eq (..), otherwise, Bool (..), undefined)
import GHC.Show (Show (..))
import Data.Tuple (uncurry)

data Polynomial f = Polynomial { toList :: [f] }
fromList l = Polynomial { toList = l }

x :: (Field f) => Polynomial f
x = fromList [zero, one]

degree :: (Field f) => Polynomial f -> Maybe Int
degree (Polynomial { toList = [] }) = Nothing
degree f = Just (length (toList f) - 1)

instance (Show f, Field f) => Show (Polynomial f) where
    show l = if null reps then "0" else foldl1 (\acc e -> acc*"+"*e) $ reverse reps where
        indexed = zip [0..] (toList l)
        repFunc (pow, coef) = if coef == zero then "" else case pow of
            0 -> show coef
            1 -> show coef * "*x"
            n -> show coef * "*x^" * show pow
        reps = fmap repFunc (filter ((/=zero) . snd) indexed)

instance (Field f) => Eq (Polynomial f) where
    (==) f g = toList f == toList g
instance (Field f) => AbelianMonoid (Polynomial f) where
    (+) f g = fromList (trim (zipWithId zero (+) (toList f) (toList g))) where
        trim l = reverse (dropWhile (==(zero)) (reverse l))
    zero = fromList []
instance (Field f) => AbelianGroup (Polynomial f) where
    neg f = fromList (fmap neg (toList f))
instance (Field f) => Semigroup (Polynomial f) where
    f * g = foldl (+) zero (fmap fromList shifts) where
        mulPow :: (Field f) => (Integer, f) -> [f] -> [f]
        mulPow (n, coef) h = [zero]^n ++ fmap (*coef) h
        shifts = do
            term <- zip [0..] (toList g)
            return (mulPow term (toList f))
instance (Field f) => Monoid (Polynomial f) where
    one = fromList [one]
instance (Field f) => Ring (Polynomial f)
instance (Field f) => EuclideanDomain (Polynomial f) where
    divMod f g = fst . head . dropWhile (uncurry (/=)) $ zip a (tail a) where
        a = iterate (func gLead) (zero, f)
        gLead = last (zip [(0 :: Int)..] (toList g))
        func (n, gcoef) (dividend, f) = let factor = (last (toList f) / gcoef) in
            case degree f of
                Nothing -> (dividend, f)
                Just d -> let shift = d - n in
                    if shift < 0 then (dividend, f) else (dividend + factor *^ x^shift, f - factor *^ x^shift*g)

(-^) :: (Field f, Integral n) => f -> n -> Polynomial f
(-^) a n = fromList ([zero]^n ++ [a])

instance Functor Polynomial where
    fmap f = fromList . fmap f . toList
instance (Field f) => VectorSpace (Polynomial f) f where
    s *^ f = fmap (*s) f

applyPoly :: (Field f, Ring v, VectorSpace v f) => Polynomial f -> v -> v
applyPoly f x = foldl1 (+) (fmap subPower (zip [(0 :: Int)..] (toList f))) where
    subPower (n, coef) = coef *^ (x^n)
