module Profunctor where

class Profunctor p where
  dimap :: (a' -> a) -> (b -> b') -> p a b -> p a' b'
  dimap f g = lmap f . rmap g

  lmap :: (a' -> a) -> p a c -> p a' c
  lmap f = dimap f id

  rmap :: (b -> b') -> p c b -> p c b'
  rmap g = dimap id g

  {-# MINIMAL dimap | lmap, rmap #-}

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

instance Profunctor (->) where
  -- dimap :: (a' -> a) -> (b -> b') -> (a -> b) -> (a' -> b')
  dimap a'toa btob' atob = btob' . atob . a'toa
