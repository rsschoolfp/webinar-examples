{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}

module State where

import Prelude (Show(show), Int, String, ($), (+), length)
import Data.Functor (Functor(fmap), (<$>))
import Control.Applicative (Applicative(pure, (<*>)))
import Control.Monad (Monad((>>=)))

data Stack a = Stack [a] deriving (Show)

push :: a -> Stack a -> (Int, Stack a)
push x (Stack xs) = (length xs + 1, Stack $ x:xs)

pop :: Stack a -> (a, Stack a)
pop (Stack (x:xs)) = (x, Stack xs)

program :: Stack String -> Stack String
program stack@(Stack xs) =
  let (len, stack1) = push "*" stack
      (_, stack2)   = push (show len) stack1
      (_, stack3)   = push "&" stack2
      (_, stack4)   = pop stack3
   in stack4

data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)
  deriving (Show, Functor)

-- instance Functor Tree where
--   fmap f (Leaf x) = Leaf $ f x
--   fmap f (Node l r) = Node (fmap f l) (fmap f r)

tree :: Tree String
tree =
  Node
    (Node
      (Node
        (Leaf "^")
        (Leaf "&"))
      (Leaf "**"))
    (Leaf "#")

numberLeaves :: Int -> Tree a -> (Tree (a, Int), Int)
numberLeaves n (Leaf x) = (Leaf (x, n), n + 1)
numberLeaves n (Node l r) =
  let (l', n')  = numberLeaves n l
      (r', n'') = numberLeaves n' r
   in (Node l' r', n'')

data State s a = State { runState :: s -> (s, a) }

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

next :: State Int Int
next = do
  n <- get
  put $ n + 1
  pure n

numberLeaves' :: Tree a -> State Int (Tree (a, Int))
numberLeaves' (Leaf x) = do
  n <- next
  pure $ Leaf (x, n)

numberLeaves' (Node l r) = Node <$> (numberLeaves' l) <*> (numberLeaves' r)
