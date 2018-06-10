{-# LANGUAGE NoImplicitPrelude #-}

module Either where

import Prelude (Show, ($), Int, String, (>), (<))
import Data.Bool (otherwise)
import Data.Functor (Functor(fmap), (<$>))
import Control.Applicative (Applicative(pure, (<*>)))
import Control.Monad (Monad((>>=)))

data Either a b
  = Left a
  | Right b
  deriving (Show)

instance Functor (Either e) where
  fmap f (Left a) = Left a
  fmap f (Right b) = Right $ f b

instance Applicative (Either e) where
  pure = Right
  (Left e)  <*> _ = Left e
  (Right f) <*> eb = f <$> eb

instance Monad (Either e) where
  (Left e) >>= _ = Left e
  (Right b) >>= f = f b

data User = User
  { name :: String
  , age  :: Int
  } deriving (Show)

type Validator a = a -> Either String a

validateAge :: Validator Int
validateAge age | age < 0   = Left "Too young"
                | age > 150 = Left "Dalai Lama"
                | otherwise = Right age

validateName :: Validator String
validateName ""   = Left "Empty name"
validateName name = Right name
