{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module Types where

import Const (Const)
import Contravariant (Contravariant (..))
import Data.Function ()
import Data.Void (absurd)
import Profunctor (Profunctor (rmap))
import Star (Star)
import Strong (Strong)

type Optic p s t a b = p a b -> p s t

type Optic' p s a = Optic p s s a a

type Lens s t a b = forall p. Strong p => Optic p s t a b

type Lens' s a = Lens s s a a

type ARepn f s t a b = Optic (Star f) s t a b

type ARepn' f s a = ARepn f s s a a

type AView r s a = ARepn' (Const r) s a

type CoerceR p = forall x. Contravariant (p x)

coercer :: Profunctor p => CoerceR p => p a b -> p a c
coercer = rmap absurd . contramap absurd

type View s a = forall p. (Strong p, CoerceR p) => Optic' p s a
