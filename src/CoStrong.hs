{-# LANGUAGE TupleSections #-}

module CoStrong where

import Profunctor (Profunctor (dimap), swap)

class Profunctor p => CoStrong p where
  unfirst :: p (a, c) (a', c) -> p a a'
  unfirst = unsecond . dimap swap swap

  unsecond :: p (c, b) (c, b') -> p b b'
  unsecond = unfirst . dimap swap swap

  {-# MINIMAL unfirst | unsecond #-}

instance CoStrong (->) where
  -- unfirst :: ((a, c) -> (a', c')) -> (a -> a')
  unfirst actoa'c' = fst . actoa'c' . (,undefined)
