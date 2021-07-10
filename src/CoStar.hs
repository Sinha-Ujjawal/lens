{-# LANGUAGE InstanceSigs #-}

module CoStar (CoStar (..)) where

import           CoStrong
import           Profunctor

newtype CoStar f a b = CoStar {runCoStar :: f a -> b}

instance Functor f => Profunctor (CoStar f) where
  -- dimap :: (a' -> a) -> (b -> b') -> CoStar f a b -> CoStar f a' b'
  dimap a'toa btob' (CoStar fatob) =
    CoStar $ \fa' -> btob' $ fatob $ fmap a'toa fa'

instance Functor f => CoStrong (CoStar f) where
  -- unfirst :: CoStar f (a, c) (a', c) -> CoStar f a a'
  unfirst (CoStar factoa'c) =
    CoStar $ \fa -> fst $ factoa'c $ fmap (\x -> (x, undefined)) fa
