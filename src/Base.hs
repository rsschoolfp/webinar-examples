{-# LANGUAGE NoImplicitPrelude #-}


module Base where

import Prelude (Show)

flip :: (a -> b -> c) -> (b -> a -> c)
flip f x y = f y x

($) :: (a -> b) -> a -> b
($) f x = f x

data Bool
  = False
  | True
  deriving (Show)

if' :: Bool -> a -> a -> a
if' False _ v = v
if' True  v _ = v
