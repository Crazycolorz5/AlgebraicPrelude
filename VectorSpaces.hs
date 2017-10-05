{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module VectorSpaces where

import Groups

class (AbelianGroup v, Field f) => VectorSpace v f where
    infixl 7 *^
    (*^) :: f -> v -> v
    (*^) a b = b ^* a
    infixl 7 ^*
    (^*) :: v -> f -> v
    (^*) a b = b *^ a

instance (Field f) => VectorSpace f f where
    (*^) a b = a * b
