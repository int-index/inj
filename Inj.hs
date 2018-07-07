{- |

An injection is a function that never maps distinct elements of the domain to
the same element of the codomain. For example, @(\\x -> x + 1)@ is an injection,
but @(\\x -> min x 0)@ is not.

Injections can be used to construct nested structures from singleton elements.

-}

{-# LANGUAGE NoImplicitPrelude,
             DefaultSignatures,
             FunctionalDependencies,
             TypeFamilies, ScopedTypeVariables, UndecidableSuperClasses,
             DataKinds, PolyKinds, GADTs, FlexibleInstances, FlexibleContexts,
             AllowAmbiguousTypes, TypeApplications, UndecidableInstances #-}

{-# OPTIONS -fno-warn-unticked-promoted-constructors #-}

module Inj (Inj(inj), Inject(inject)) where

data IsSame = Same | Different

type family Cmp a b :: IsSame where
  Cmp a a = Same
  Cmp a b = Different

data SIsSame (s :: IsSame) where
  SSame :: SIsSame Same
  SDifferent :: SIsSame Different

class KIsSame s where
  scmp :: SIsSame s

instance KIsSame Same where
  scmp = SSame

instance KIsSame Different where
  scmp = SDifferent

type family InjDiff r where
  InjDiff Same = (~)
  InjDiff Different = Inj

class Inject a b where
  inject :: a -> b

instance (KIsSame (Cmp a b), InjDiff (Cmp a b) a b) => Inject a b where
  inject =
    case scmp @(Cmp a b) of
      SSame -> \x -> x
      SDifferent -> inj

-- | Inject @p@ into @a@.
--
-- By convention, the instances of @Inj@ never match on @p@ and always match on
-- @a@. This guarantees that the users will not encounter overlapping instances.
class Inj p a where
  -- | Inject @p@ into @a@.
  inj :: Cmp p a ~ Different => p -> a
