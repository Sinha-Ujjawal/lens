module Strong where

import           Profunctor

class Profunctor p => Strong p where
  first :: p a a' -> p (a, c) (a', c)
  first = dimap swap swap . second

  second :: p b b' -> p (c, b) (c, b')
  second = dimap swap swap . first

  {-# MINIMAL first | second #-}

instance Strong (->) where
  -- first :: (a -> a') -> (a, c) -> (a', c')
  first atoa' (a, c) = (atoa' a, c)
