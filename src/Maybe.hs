{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}

module Maybe where

import Prelude (Show)

import Data.Semigroup (Semigroup((<>)))
import Data.Monoid (Monoid(mempty, mappend), (<>))
import Data.Functor (Functor(fmap))
import Control.Applicative (Applicative(pure, (<*>)))
import Control.Monad (Monad((>>=)))

data Maybe a
  = Nothing
  | Just a
  deriving (Show)

instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just (f x)

instance (Semigroup a) => Semigroup (Maybe a) where
  (<>) a Nothing = a
  (<>) Nothing b = b
  (<>) (Just a) (Just b) = Just (a <> b)

instance (Monoid a) => Monoid (Maybe a) where
  mempty = Nothing

instance Applicative Maybe where
  pure = Just
  Just f  <*> m = fmap f m
  Nothing <*> _ = Nothing

instance Monad Maybe where
  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  Nothing >>= f = Nothing
  Just a  >>= f = f a

maybe :: b -> (a -> b) -> Maybe a -> b
maybe fallback f mA =
  case mA of
    Nothing -> fallback
    Just x  -> f x
