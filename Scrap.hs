

--class (Category cat, obj) => TwoCategory cat obj where
--    (.+) :: cat (cat a b) (cat a b) ->

class (Category c obj) => TwoCategory c obj homset where --The homset must be equaitable
    (.+) :: (homset (c a b) (c a b)) -> (homset (c a b) (c a b)) -> (homset (c a b) (c a b))

instance TwoCategory (->) Empty (->) where
    (.+) :: ((a->b) -> (a->b)) -> ((a->b) -> (a->b)) -> ((a->b) -> (a->b))


{-
class (Category cat obj) => EnrichedCategory (cat :: k -> k -> *) (obj :: * -> Constraint) (morph :: * -> Constraint) where
    (.+) :: (obj a, obj b, obj c, morph (cat a b), morph (cat b c), morph (cat a c)) => cat b c -> cat a b -> cat a c
    id' :: (obj a, morph (cat a a)) => cat a a

instance (Category cat obj) => EnrichedCategory cat obj Category
-}

class (Category c Empty) => Arrow c where
arr :: (a -> b) -> c a b

instance Arrow (->) where
arr = id
{-
class (Category c) => Functor f c where
fmap :: c a b -> c (f a) (f b)

instance Functor [] (->) where
fmap f [] = []
fmap f (x:xs) = f x : fmap f xs
-}

{-
class (Category cat1, Category cat2) => Functor f cat1 cat2 where
fmap :: cat1 a b -> cat2 (f a) (f b)

instance (Arrow cat1, Arrow cat2, Arrow cat3, Functor f cat2 cat3, Functor g cat1 cat2) => Functor (Composition f g) cat1 cat3 where
fmap f = arr Compose . fmap fmap f . arr unCompose
-}


--instance Functor Id

{-
type Endofunctor f = forall a . f a
instance (Functor f) => Functor (Endofunctor f) where
fmap f = Endofunctor . fmap f . unEndo
-}
--data NTProd f g h j = NTProd (forall x . (f x -> g x, h x -> j x))
--makeNTProd :: NaturalTransformation f g -> NaturalTransformation h j -> NTProd f g h j
--makeNTProd a b = NTProd (getNT a, getNT b)
{-
newtype FunctorProd f g x = FunctorProd { getFunctorPair :: (Functor f, Functor g) => (f x, g x) }

instance (Functor f, Functor g) => Functor (FunctorProd f g) where
    fmap f (FunctorProd x) = let (a, b) = x in FunctorProd { getFunctorPair = (fmap f a, fmap f b) }
-}

class (Category c obj) => Product c obj p where
    pi1 :: c (p a b) a
    pi2 :: c (p a b) b
    makeProd :: c d a -> c d b -> c d (p a b)


instance Product (->) Empty (,) where
    pi1 (a, b) = a
    pi2 (a, b) = b
    makeProd f g = \x -> (f x, g x)

instance (Category cat obj, Product cat obj p) => Endobifunctor cat obj p where
    appBF a b = let bLine = a . pi1
                    dLine = b . pi2
                in makeProd bLine dLine

{-
instance Product NaturalTransformation Functor FunctorProd where
pi1 = NaturalTransformation (pi1 . getFunctorPair)
pi2 = NaturalTransformation (pi2 . getFunctorPair)
makeProd (NaturalTransformation f) (NaturalTransformation g) =
NaturalTransformation (\x -> FunctorProd (f x, g x))
-}
