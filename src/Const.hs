module Const
  ( Const (..),
  )
where

import Contravariant (Contravariant (..))

newtype Const a b = Const {getConst :: a} deriving (Show, Eq)

instance Functor (Const a) where
  -- fmap :: (x -> y) -> Const a x -> Const a
  fmap _ (Const a) = Const a

instance Contravariant (Const x) where
  -- contramap :: (b -> a) -> Const x a -> Const x b
  contramap _ (Const x) = Const x
