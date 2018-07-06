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

-- @instance Inj a a@ is tempting to define. Unfortunately, it does not work
-- as well as one might hope. For instance, consider a type like this:
--
-- @
-- data Shape x = Circle | Rectangle | Other x
-- @
--
-- If we want to write @inj Circle@, then we get an ambiguity error:
--
-- @
--    * Could not deduce (Inj (Shape x0) (Shape x))
--        arising from a use of `inj'
-- @
--
-- That is because @Inj a a@ for @Shape x@ is equivalent to
--
-- @instance Inj (Shape x) (Shape x)@
--
-- but for good type inference we want
--
-- @instance (p ~ Shape x) => Inj p (Shape x)@
--
-- Unfortunately, this instance can't be used in the presence of @Inj a a@
-- due to overlap.
