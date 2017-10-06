{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds #-}

module Categories (module Categories) where

import Groups (Semigroup (..), Monoid (..))
import GHC.Base (Eq (..))
import Data.Tuple (curry)

class Category (cat :: k -> k -> *) obj  | cat -> obj where
    infixr 9 .
    (.) :: (obj c, obj d, obj b) => cat c d -> cat b c -> cat b d
    id :: (obj a) => cat a a

class (Category cat obj) => CartesianClosed cat obj where
    evaluationMap :: cat (cat a b, a) b
    --Functor for exponentials?

infixr 0 $
($) :: (a -> b) -> a -> b
($) = curry evaluationMap

class Functor f where
    fmap :: (a -> b) -> (f a -> f b)

infixl 4 <$>
(<$>) :: (Functor f) => (a -> b) -> (f a -> f b)
(<$>) = fmap

--class ContravariantFunctor f where
--    contramap :: (a -> b) -> (f b -> f a)

class IdentityFunctor f where
    appId :: a -> f a
    unId :: f a -> a

--class Endofunctor f where
--    endoMap :: c a b -> c (f a) (f b)

data Composition f g a = Compose { unCompose :: f (g a) } --Data.Functor.Compose
liftComp :: (f (g a) -> y) -> Composition f g a -> y
liftComp f = f . unCompose

instance (Functor f, Functor g) => Functor (Composition f g) where
    fmap f = Compose . fmap (fmap f) . unCompose

data Id a = Id a

instance IdentityFunctor Id where
    appId = Id
    unId (Id x) = x
instance Functor Id where
    fmap f = appId . f . unId

class (Category cat obj) => Endobifunctor cat obj bf where
    appBF :: (obj a, obj b, obj c, obj d, obj (bf a c), obj (bf b d)) => cat a b -> cat c d -> cat (bf a c) (bf b d)

class (Category cat obj, Unitary cat obj o, Endobifunctor cat obj bi) => MonoidalCategory cat obj bi o | bi o -> cat where
    assoc :: (obj (bi (bi a b) c), obj (bi a (bi b c)), obj a, obj b, obj c) => cat (bi (bi a b) c) (bi a (bi b c))
    lunit :: (obj (bi o a), obj o, obj a) => cat (bi o a) a
    runit :: (obj (bi a o), obj o, obj a) => cat (bi a o) a

class (Category c obj) => Unitary c obj o where
    unitId :: (obj o) => c o o

class (MonoidalCategory c obj bi o) => CMonoid c obj bi o (m :: k) | m -> c obj bi o where
    mult :: (obj (bi m m), obj m) => c (bi m m) m
    unit :: (obj o, obj m) => c o m

--Category of Endofunctors with NaturalTransformations as arrows:
--https://github.com/ku-fpg/natural-transformation/blob/master/src/Control/Natural.hs
data NaturalTransformation (f :: * -> *) (g :: * -> *) = (Functor f, Functor g) => NaturalTransformation { getNT :: forall x . f x -> g x }
instance Category NaturalTransformation Functor where
    (NaturalTransformation a) . (NaturalTransformation b) = NaturalTransformation (a . b)
    id = NaturalTransformation id
instance Unitary NaturalTransformation Functor Id where
    unitId = id
instance Endobifunctor NaturalTransformation Functor Composition where
    appBF (NaturalTransformation f) (NaturalTransformation g) = NaturalTransformation (Compose . f . fmap g . unCompose)
instance MonoidalCategory NaturalTransformation Functor Composition Id where
    assoc = NaturalTransformation (Compose . fmap Compose . unCompose . unCompose)
    lunit = NaturalTransformation (unId . unCompose)
    runit = NaturalTransformation (fmap unId . unCompose)
class (Functor m, CMonoid NaturalTransformation Functor Composition Id m) => Monad m where
    --Literally just the monoid operations for a monoid in the category of endofunctors under composition
    eta :: NaturalTransformation Id m
    eta = unit
    mu :: NaturalTransformation (Composition m m) m
    mu = mult
    --Map to the inside then apply the mu transformation
    infixl 1 >>=
    (>>=) :: m a -> (a -> m b) -> m b
    m >>= f = (getNT mu) (Compose (fmap f m))
    return :: a -> m a
    return = getNT eta . appId

--Category of Types with Functions as arrows:
--The empty constraint.
class Empty a
instance Empty a
instance Category (->) Empty where
    f . g = \x -> f (g x)
    id = \x -> x
instance CartesianClosed (->) Empty where
    evaluationMap (f, a) = f a
data Unit = Unit
newtype Monoidal a = Monoidal a
instance IdentityFunctor Monoidal where
    appId = Monoidal
    unId (Monoidal a) = a
instance Unitary (->) Empty Unit where
    unitId = id
instance Endobifunctor (->) Empty (,) where
    appBF f g = \(a,b) -> (f a, g b)
instance MonoidalCategory (->) Empty (,) Unit where
    assoc ((a,b),c) = (a,(b,c))
    lunit (Unit, a) = a
    runit (a, Unit) = a
{-
instance (CMonoid (->) Empty (,) Unit a) => Monoid (Monoidal a) where
    (*) (Monoidal a) (Monoidal b) = Monoidal (mult (a,b))
    one = Monoidal (unit Unit)
-}
instance (Eq e) => Eq (Monoidal e) where
    a == b = unId a == unId b
instance Monoid a => CMonoid (->) Empty (,) Unit (Monoidal a) where
    mult (Monoidal a, Monoidal b) = Monoidal (a*b)
    unit _ = Monoidal one
instance Monoid a => Semigroup (Monoidal a) where
    (*) (Monoidal a) (Monoidal b) = Monoidal (a*b)
instance Monoid a => Monoid (Monoidal a) where
    one = Monoidal one
