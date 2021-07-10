{-# LANGUAGE FlexibleInstances #-}

module Contravariant where

class Contravariant f where
  contramap :: (b -> a) -> f a -> f b
