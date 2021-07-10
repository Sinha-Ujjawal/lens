{-# LANGUAGE FlexibleContexts #-}

module Star
  ( Star(..)
  ) where

import           Contravariant
import           Profunctor
import           Strong

-- a -> f b means a is sort of playing the role of f b
-- that means, given a "a" there exist a "b" (not necessarily unique)
-- such that; a -> f b exists
newtype Star f a b = Star {runStar :: a -> f b}

instance Functor f => Profunctor (Star f) where
  -- dimap :: (a' -> a) -> (b -> b') -> Star f a b -> Star f a' b'
  dimap a'toa btob' (Star atofb) = Star $ \a' -> fmap btob' $ atofb $ a'toa a'

instance Functor f => Strong (Star f) where
  -- first :: Star f a a' -> Star f (a, c) (a', c)
  first (Star atofa') = Star $ \(a, c) -> flip (,) c <$> atofa' a

instance Contravariant f => Contravariant (Star f x) where
  -- contramap :: (b -> a) -> Star f x a -> Star f x b
  contramap btoa (Star xtofa) = Star $ contramap btoa . xtofa
