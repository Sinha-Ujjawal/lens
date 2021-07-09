{-# LANGUAGE RankNTypes #-}

module Lens where

import           Forget
import           Profunctor

type Optic p s t a b = p a b -> p s t
type Lens s t a b = forall p . Strong p => Optic p s t a b
type Lens' s a = Lens s s a a

-- Strong => p a b -> p s t
-- p a b -> p s b via lmap getter
-- p s b -> p (s, s') (b, s') via first
-- p (s, s') (b, s') -> p (s, s) (b, s) -> p s (b, s) via lmap dup -- (implicit type conversion, decided s' = s)
-- p s (b, s) -> p s t via rmap (\(b, s) -> setter s b)
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter =
  rmap (\(b, s) -> setter s b) . lmap dup . first . lmap getter
 where
  dup :: x -> (x, x)
  dup x = (x, x)

over :: (Strong p) => Lens s t a b -> p a b -> p s t
over = ($)

set :: Lens s t a b -> b -> s -> t
set lens b = over lens (const b)

-- Lens s t a b :: forall p. Strong p => p a b -> p s t
-- We get to choose the Strong Profunctor (because of forall quantifier)
-- So we choose Forget Profunctor, you will get to see why Forget Profunctor is useful here
-- Forget r a b -> Forget r s t
-- Forget (a -> r) -> Forget (s -> r)  ------ (expanding defination of Forget r a b)
-- Now, if we can only get the RHS function of the equation
-- In order to do that we will need an instance of Forget r a b (so that we can pass that to the function and extract RHS)
-- But wait, we can choose r as well. Why not pick a
-- Forget a a b -> Forget a s t
-- Forget (a -> a) -> Forget (s -> a)
-- Ah.., not it's the familiar identity function on the LHS
-- We were able to do all this because of the forall quantifier on p in Lens s t a b, and r in Forget r a b
view :: (Forget a a b -> Forget a s t) -> s -> a
view getter = runForget $ getter $ Forget id

mapped :: Functor f => Optic (->) (f a) (f b) a b
mapped = (<$>)

_1 :: Lens (a, c) (b, c) a b
_1 = lens getter setter
 where
  getter = fst
  setter (_, b) a = (a, b)

_2 :: Lens (c, a) (c, b) a b
_2 = lens getter setter
 where
  getter = snd
  setter (a, _) b = (a, b)
