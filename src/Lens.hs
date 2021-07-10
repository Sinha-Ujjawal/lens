{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FlexibleContexts #-}

module Lens where

import           Const
import           Contravariant
import           Profunctor
import           Star
import           Strong
import           Types


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

over :: (Strong p) => Optic p s t a b -> p a b -> p s t
over = ($)

set :: Optic (->) s t a b -> b -> s -> t
set lens b = over lens (const b)

-- AView r s a :: ARepn' (Const r) s a
-- In Our case, r is a
-- AView a s a :: ARepn' (Const a) s a
-- ARepn' f s a = ARepn f s s a a
-- ARepn f s t a b = Optic (Star f) s t a b
-- Optic p s t a b = p a b -> p s t
-- Star f a b = Star {runStar :: a -> f b}
-- Const a b = Const {getConst :: a}
-- therefore, ARepn' f s a = Optic (Star f) s s a a
-- ARepn' f s a = Star f a a -> Star f s s
-- AView r s a = Star (Const r) a a -> Star (Const r) s s
-- AView a s a = Star (Const a) a a -> Star (Const a) s s
-- AView a s a = (Star {runStar :: a -> Const a a}) -> (Star {runStar :: s -> Const s s})
-- If only we had an instance of Star {runStar :: a -> Const a a}
-- And, we do
-- a -> Const a a, can be broken down to a -> Const {getConst :: a}, well that's just Const
-- we can then Wrap this around Star
-- therefore, Star Const :: forall a. Star {runStar :: a -> Const a a'}
view :: AView a s a -> s -> a
view getter = getConst . runStar (getter (Star Const))

-- View s a = forall p . (Strong p, CoerceR p) => Optic' p s a
-- Optic' p s a = p a a -> p s s
-- CoerceR p = forall x . Contravariant (p x)
to :: (s -> a) -> View s a
to f = coercer . lmap f

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
