{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}

module Lazy where

import Prelude ((.), ($), const)
import Data.Functor (Functor(fmap))
import Control.Applicative (Applicative(pure, (<*>)))
import Control.Monad (Monad((>>=)))

data Lazy r a = Lazy { unLazy :: r -> a }

instance Functor (Lazy r) where
  fmap :: (a -> b) -> Lazy r a -> Lazy r b
  fmap f (Lazy l) = Lazy $ f . l

instance Applicative (Lazy r) where
  pure :: a -> Lazy r a
  pure = Lazy . const

  (<*>) :: Lazy r (a -> b) -> Lazy r a -> Lazy r b
  Lazy ff <*> Lazy fa = Lazy $ \r -> (ff r) (fa r)

instance Monad (Lazy r) where
  (>>=) :: Lazy r a -> (a -> Lazy r b) -> Lazy r b
  Lazy fa >>= f = Lazy $ \r -> (unLazy $ f (fa r)) r
