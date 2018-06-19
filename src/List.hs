{-# LANGUAGE NoImplicitPrelude #-}

module List where

import Prelude (Int, (+), const, ($), Show(show), Functor(fmap), (++), (.), (<$>))
import Data.Semigroup (Semigroup((<>)))
import Data.Monoid (Monoid(mempty))
import Control.Applicative (Applicative(pure, (<*>)))
import Control.Monad (Monad((>>=)), (>=>), foldM)
import Data.Foldable (Foldable(foldr), foldl, foldMap)

import Base (Bool, flip, if')
import Maybe (Maybe(..))

data List a
  = Empty
  | Cons a (List a)

instance (Show a) => Show (List a) where
  show list = "[" ++ go list ++ "]"
    where
      go Empty = ""
      go (Cons h Empty) = show h
      go (Cons h t) = show h ++ ", " ++ go t

instance Foldable List where
  foldr _ acc Empty = acc
  foldr f acc (Cons h t) = f h (foldr f acc t)

instance Semigroup (List a) where
  (<>) = concat

instance Monoid (List a) where
  mempty = Empty

instance Functor List where
  fmap = map

instance Applicative List where
  pure a = Cons a Empty
  (<*>) lf la =
    foldl (\acc a -> foldl (\acc' f -> Cons (f a) acc')
                     acc
                     lf)
          Empty
          la

instance Monad List where
  (>>=) = flip concatMap

l3 :: List Int
l3 = Cons 2 (Cons 5 (Cons 9 Empty))

infixr 5 %

(%) :: a -> List a -> List a
(%) = Cons

l4 :: List Int
-- l4 = Cons 4 (Cons 7 (Cons 11 (Cons 13 Empty)))
l4 = 4 % 7 % 11 % 13 % Empty

-- l3' = (+) 2 ((+) 5 ((+) 9 0))

-- length :: List a -> Int
-- length Empty      = 0
-- length (Cons _ t) = 1 + length t

-- sum :: List Int -> Int
-- sum = foldl (+) 0

length :: List a -> Int
length = foldr (const $ (+) 1) 0

prepend :: a -> List a -> List a
prepend = Cons

append :: a -> List a -> List a
append x Empty = Cons x Empty
append x (Cons h t) = Cons h (append x t)

reverse :: List a -> List a
reverse = foldl (flip prepend) Empty

map :: (a -> b) -> List a -> List b
map f = foldr (Cons . f) Empty

filter :: (a -> Bool) -> List a -> List a
filter f = foldr (\x acc -> if' (f x) (Cons x acc) acc) Empty

concat :: List a -> List a -> List a
concat listA listB = foldr prepend listB listA

concatMap :: (a -> List b) -> List a -> List b
concatMap = foldMap

head :: List a -> Maybe a
head Empty      = Nothing
head (Cons h _) = Just h

tail :: List a -> Maybe (List a)
tail Empty      = Nothing
tail (Cons _ t) = Just t

tail2 :: List a -> Maybe (List a)
tail2 list =
  case tail list of
    Nothing -> Nothing
    Just t  -> tail t

tail2' :: List a -> Maybe (List a)
tail2' = tail >=> tail
