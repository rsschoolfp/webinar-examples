{-# LANGUAGE NoImplicitPrelude #-}

module Function where

import Prelude (($), (.), const)
import Data.Functor (Functor(fmap))
import Control.Applicative (Applicative(pure, (<*>)))
import Control.Monad (Monad((>>=)))

-- ((->) a)

newtype Function t a = Function (t -> a)

instance Functor (Function t) where
  fmap f (Function g) = Function $ f . g

instance Applicative (Function t) where
  pure = Function . const
  -- (t -> (a -> b)) -> (t -> a) -> (t -> b)
  -- laws??
  (<*>) (Function ff) (Function fa) = Function $ \t -> (ff t) (fa t)

instance Monad (Function t) where
  (>>=) (Function fa) f = Function $ \t ->
    let (Function fb) = f (fa t) in fb t
