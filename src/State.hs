{-# LANGUAGE NoImplicitPrelude #-}

module State where

import Prelude (Show, Int, ($), (+), length)
import Data.Functor (Functor(fmap))
import Control.Applicative (Applicative(pure, (<*>)))
import Control.Monad (Monad((>>=)))

data State s a = State { runState :: s -> (s, a) }

data Stack a = Stack [a] deriving (Show)

push :: a -> State (Stack a) Int
push x = do
  Stack xs <- get
  put $ Stack $ x:xs
  pure $ length xs + 1

pop :: State (Stack a) a
pop = do
  Stack (x:xs) <- get
  put $ Stack xs
  pure x

instance Functor (State s) where
  fmap f (State sf) =
    State $ \s -> let (s', a) = sf s in (s', f a)

instance Applicative (State s) where
  pure a = State $ \s -> (s, a)
  State sff <*> State sfa =
    State $ \s ->
      let (s', f)  = sff s
          (s'', a) = sfa s'
       in (s'', f a)

instance Monad (State s) where
  State sf >>= f =
    State $ \s ->
      let (s', a)   = sf s
          State sf' = f a
       in sf' s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put ns = State $ \_ -> (ns, ())

modify :: (s -> s) -> State s ()
modify f = do
  s <- get
  put $ f s

gets :: (s -> a) -> State s a
gets f = do
  s <- get
  pure $ f s

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

t :: Tree Int
t = Node (Node (Leaf 37) (Node (Leaf 1) (Leaf 2))) (Node (Leaf 3) (Leaf 4))

f :: Int -> Tree Int -> (Tree (Int, Int), Int)
f n (Leaf x) = (Leaf (x, n), n + 1)
f n (Node l r) =
  let (l', n') = f n l
      (r', n'') = f n' r
   in (Node l' r', n'')
