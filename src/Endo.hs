module Endo where

import Data.Semigroup (Semigroup((<>)))
import Data.Monoid (Monoid(mempty))

newtype Endo a = Endo { appEndo :: a -> a }

instance Semigroup (Endo a) where
  (<>) (Endo f) (Endo g) = Endo $ f . g

instance Monoid (Endo a) where
  mempty = Endo id
