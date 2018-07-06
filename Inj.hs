{- |

An injection is a function that never maps distinct elements of the domain to
the same element of the codomain. For example, @(\\x -> x + 1)@ is an injection,
but @(\\x -> min x 0)@ is not.

Injections can be used to construct nested structures from singleton elements.

-}

{-# LANGUAGE NoImplicitPrelude,
             DefaultSignatures,
             MultiParamTypeClasses,
             TypeFamilies #-}

module Inj (Inj(..)) where

-- | Inject @p@ into @a@.
--
-- By convention, the instances of @Inj@ never match on @p@ and always match on
-- @a@. This guarantees that the users will not encounter overlapping instances.
class Inj p a where
  -- | Inject @p@ into @a@.
  inj :: p -> a

  default inj :: (p ~ a) => p -> a
  inj = \x -> x
