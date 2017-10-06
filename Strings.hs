module Strings where
{-# LANGUAGE NoImplicitPrelude #-}

import Groups
import GHC.Show (Show)

instance Semigroup [a] where
    (*) a b = case a of
        [] -> b
        (x:xs) -> x : xs * b
instance Monoid [a] where
    one = []
