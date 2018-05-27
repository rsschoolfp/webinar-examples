{-# LANGUAGE NoImplicitPrelude #-}

module NonEmpty where

import Prelude (Int, Foldable(foldr), ($), Show, Functor(fmap), (<$>), (+))
import Data.Semigroup (Semigroup((<>)))

import List (List(Empty), (%))
import qualified List

data NonEmpty a = NonEmpty a (List a) deriving (Show)

single :: a -> NonEmpty a
single x = NonEmpty x Empty

ne_1 :: NonEmpty Int
ne_1 = single 37

ne_3 :: NonEmpty Int
ne_3 = NonEmpty 2 (3 % 5 % 7 % 11 % Empty)

head :: NonEmpty a -> a
head (NonEmpty x _) = x

tail :: NonEmpty a -> List a
tail (NonEmpty _ xs) = xs

length :: NonEmpty a -> Int
length (NonEmpty _ xs) = 1 + List.length xs

instance Foldable NonEmpty where
  foldr f acc (NonEmpty x xs) = foldr f (f x acc) xs

instance Semigroup (NonEmpty a) where
  (<>) (NonEmpty a as) (NonEmpty b bs) = NonEmpty a (List.concat as $ b % bs)

instance Functor NonEmpty where
  fmap f (NonEmpty x xs) = NonEmpty (f x) (f <$> xs)
