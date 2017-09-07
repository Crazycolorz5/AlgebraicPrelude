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

module Categories (Empty, Category (..), Functor (..), Composition (..), liftComp,
                   Id, unId, NaturalTransformation (..), MonoidalCategory (..),
                   CMonoid (..), Monad (..), Endobifunctor (..))

where

class Category (cat :: k -> k -> *) obj  | cat -> obj where
    (.) :: (obj c, obj d, obj b) => cat c d -> cat b c -> cat b d
    id :: (obj a) => cat a a
{-
class (Category cat) => CartesianClosed cat where
    evaluationMap :: cat (a->b, a) b
    --Functor for exponentials?
-}
--The empty constraint.
class Empty a
instance Empty a

instance Category (->) Empty where
    f . g = \x -> f (g x)
    id = \x -> x

class Functor f where
    fmap :: (a -> b) -> (f a -> f b)

--class Endofunctor f where
--    endoMap :: c a b -> c (f a) (f b)

data Composition f g a = Compose { unCompose :: f (g a) } --Data.Functor.Compose
liftComp :: (f (g a) -> y) -> Composition f g a -> y
liftComp f = f . unCompose

instance (Functor f, Functor g) => Functor (Composition f g) where
    fmap f = Compose . fmap (fmap f) . unCompose

data Id a = Id a
unId :: Id a -> a
unId (Id x) = x

instance Functor Id where
    fmap f (Id x) = Id (f x)

--https://github.com/ku-fpg/natural-transformation/blob/master/src/Control/Natural.hs
data NaturalTransformation (f :: * -> *) (g :: * -> *) = (Functor f, Functor g) => NaturalTransformation { getNT :: forall x . f x -> g x }

instance Category NaturalTransformation Functor where
    (NaturalTransformation a) . (NaturalTransformation b) = NaturalTransformation (a . b)
    id = NaturalTransformation id

class (Category cat obj) => Endobifunctor cat obj bf where
    appBF :: (obj a, obj b, obj c, obj d, obj (bf a c), obj (bf b d)) => cat a b -> cat c d -> cat (bf a c) (bf b d)

class (Category cat obj, Unitary cat obj o, Endobifunctor cat obj bi) => MonoidalCategory cat obj bi o | bi o -> cat --where
--    assoc :: (obj (bi (bi a b) c), obj (bi a (bi b c))) => cat (bi (bi a b) c) (bi a (bi b c))
--    lunit :: (obj (bi o a), obj a) => cat (bi o a) a
--    runit :: (obj (bi a o), obj a) => cat (bi a o) a

class (Category c obj) => Unitary c obj o where
    unitId :: (obj o) => c o o

class (MonoidalCategory c obj bi o) => CMonoid c obj bi o (m :: k) | m -> c bi o where
    mult :: (obj (bi m m), obj m) => c (bi m m) m
    unit :: (obj o, obj m) => c o m

instance Unitary NaturalTransformation Functor Id where
    unitId = id

instance Endobifunctor NaturalTransformation Functor Composition where
    appBF (NaturalTransformation f) (NaturalTransformation g) = NaturalTransformation (Compose . f . fmap g . unCompose)

instance MonoidalCategory NaturalTransformation Functor Composition Id

class (Functor m, CMonoid NaturalTransformation Functor Composition Id m) => Monad m where
    --Literally just the monoid operations for a monoid in the category of endofunctors under composition
    eta :: NaturalTransformation Id m
    eta = unit
    mu :: NaturalTransformation (Composition m m) m
    mu = mult
    --Map to the inside then apply the mu transformation
    (>>=) :: m a -> (a -> m b) -> m b
    m >>= f = (getNT mu) (Compose (fmap f m))
