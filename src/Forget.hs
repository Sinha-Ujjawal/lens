module Forget where

import           Profunctor

data Forget r a b = Forget {runForget :: (a -> r)}

instance Profunctor (Forget r) where
  -- dimap :: (a' -> a) -> (b -> b') -> Forget r a b -> Forget r a' b'
  dimap a'toa _ (Forget ator) = Forget (ator . a'toa)

instance Strong (Forget r) where
  -- first :: Forget r a a' -> Forget r (a, c) (a', c)
  first (Forget ator) = Forget (ator . fst)
