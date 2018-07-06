-- | 'Inj' instances for types from 'base'.

{-# LANGUAGE
    DefaultSignatures,
    MultiParamTypeClasses,
    FlexibleInstances,
    FlexibleContexts,
    TypeFamilies,
    ScopedTypeVariables,
    DataKinds,
    TypeOperators,
    UndecidableInstances
#-}

module Inj.Base () where

import GHC.TypeLits
import Control.Applicative
import Control.Exception hiding (TypeError)
import Control.Monad.ST
import Data.Complex
import Data.Fixed
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Int
import Data.List.NonEmpty
import Data.Monoid
import Data.Ord
import Data.Proxy
import Data.Semigroup
import Data.Word
import Foreign.Ptr
import GHC.Conc
import GHC.Generics
import GHC.Real
import Numeric.Natural
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec

import Inj

--------------------------------------------------------------------------------
-- 'fromIntegral' is an injection from any @Integral p@ to any @Num a@.
--------------------------------------------------------------------------------

instance Integral p => Inj p Integer where
  inj = toInteger

-- | Throws 'Underflow'.
instance Integral p => Inj p Natural where
  inj = fromIntegral

fromIntegralBounded ::
  forall p a. (Integral a, Bounded a) => Integral p => p -> a
fromIntegralBounded p
  | p' < toInteger (minBound :: a) = throw Underflow
  | p' > toInteger (maxBound :: a) = throw Overflow
  | otherwise = fromInteger p'
  where
    p' = toInteger p

-- | Throws 'Underflow' and 'Overflow'.
instance Integral p => Inj p Int where
  inj = fromIntegralBounded

-- | Throws 'Underflow' and 'Overflow'.
instance Integral p => Inj p Int8 where
  inj = fromIntegralBounded

-- | Throws 'Underflow' and 'Overflow'.
instance Integral p => Inj p Int16 where
  inj = fromIntegralBounded

-- | Throws 'Underflow' and 'Overflow'.
instance Integral p => Inj p Int32 where
  inj = fromIntegralBounded

-- | Throws 'Underflow' and 'Overflow'.
instance Integral p => Inj p Int64 where
  inj = fromIntegralBounded

-- | Throws 'Underflow' and 'Overflow'.
instance Integral p => Inj p Word where
  inj = fromIntegralBounded

-- | Throws 'Underflow' and 'Overflow'.
instance Integral p => Inj p Word8 where
  inj = fromIntegralBounded

-- | Throws 'Underflow' and 'Overflow'.
instance Integral p => Inj p Word16 where
  inj = fromIntegralBounded

-- | Throws 'Underflow' and 'Overflow'.
instance Integral p => Inj p Word32 where
  inj = fromIntegralBounded

-- | Throws 'Underflow' and 'Overflow'.
instance Integral p => Inj p Word64 where
  inj = fromIntegralBounded

-- | Throws 'Underflow' and 'Overflow'.
instance Integral p => Inj p IntPtr where
  inj = fromIntegralBounded

-- | Throws 'Underflow' and 'Overflow'.
instance Integral p => Inj p WordPtr where
  inj = fromIntegralBounded

--------------------------------------------------------------------------------
-- 'realToFrac' is an injection from any @Real p@ to any @Fractional a@.
--------------------------------------------------------------------------------

instance (Inj Integer a, Real p) => Inj p (Ratio a) where
  inj p = inj n :% inj d
    where
      n :% d = toRational p

-- | Throws 'LossOfPrecision'.
instance (HasResolution res, Real p) => Inj p (Fixed res) where
  inj p
    | denominator i == 1 = MkFixed (numerator i)
    | otherwise = throw LossOfPrecision
    where
      i = toRational p * toRational res
      res = resolution (Proxy :: Proxy res)

-- | Injective only if the number is representable as 'Float'.
instance Real p => Inj p Float where
  inj = realToFrac

-- | Injective only if the number is representable as 'Double'.
instance Real p => Inj p Double where
  inj = realToFrac

instance (Num a, Inj p a) => Inj p (Complex a) where
  inj p = inj p :+ 0

--------------------------------------------------------------------------------
-- 'pure' is often an injection into @Applicative f@.
--------------------------------------------------------------------------------

instance Inj p a => Inj p [a] where
  inj = pure . inj

instance Inj p a => Inj p (Maybe a) where
  inj = pure . inj

instance Inj p a => Inj p (IO a) where
  inj = pure . inj

instance Inj p a => Inj p (NonEmpty a) where
  inj = pure . inj

instance Inj p a => Inj p (ReadP a) where
  inj = pure . inj

instance Inj p a => Inj p (ReadPrec a) where
  inj = pure . inj

instance Inj p a => Inj p (Down a) where
  inj = pure . inj

instance Inj p a => Inj p (Product a) where
  inj = pure . inj

instance Inj p a => Inj p (Sum a) where
  inj = pure . inj

instance Inj p a => Inj p (Dual a) where
  inj = pure . inj

instance Inj p a => Inj p (Data.Monoid.Last a) where
  inj = pure . inj

instance Inj p a => Inj p (Data.Monoid.First a) where
  inj = pure . inj

instance Inj p a => Inj p (STM a) where
  inj = pure . inj

instance Inj p a => Inj p (Identity a) where
  inj = pure . inj

instance Inj p a => Inj p (ZipList a) where
  inj = pure . inj

instance Inj p a => Inj p (Option a) where
  inj = pure . inj

instance Inj p a => Inj p (Data.Semigroup.Last a) where
  inj = pure . inj

instance Inj p a => Inj p (Data.Semigroup.First a) where
  inj = pure . inj

instance Inj p a => Inj p (Max a) where
  inj = pure . inj

instance Inj p a => Inj p (Min a) where
  inj = pure . inj

instance
    TypeError
      ('Text "Refusing to decide whether to inject " ':<>:
       'ShowType p ':<>: 'Text " into 'Left' " ':<>:
       'ShowType x ':<>: 'Text " or 'Right' " ':<>: 'ShowType y ':$$:
       'Text "in the " ':<>: 'ShowType Inj ':<>:
       'Text " instance for " ':<>: 'ShowType Either) =>
    Inj p (Either x y)
  where
    inj = error "impossible"

instance
    TypeError
      ('Text "Refusing to decide whether to inject " ':<>:
       'ShowType p ':<>: 'Text " into 'fst' " ':<>:
       'ShowType x ':<>: 'Text " or 'snd' " ':<>: 'ShowType y ':$$:
       'Text "in the " ':<>: 'ShowType Inj ':<>:
       'Text " instance for " ':<>: 'ShowType (,)) =>
    Inj p ((,) x y)
  where
    inj = error "impossible"

instance Inj p a => Inj p (ST s a) where
  inj = pure . inj

instance Inj p a => Inj p (r -> a) where
  inj = pure . inj

instance Inj p (f (g a)) => Inj p (Compose f g a) where
  inj = Compose . inj

--------------------------------------------------------------------------------
-- Generic
--------------------------------------------------------------------------------

instance Inj p (f a) => Inj p (Rec1 f a) where
  inj = Rec1 . inj

instance Inj p (f a) => Inj p (M1 i c f a) where
  inj = M1 . inj

instance Inj p a => Inj p (Par1 a) where
  inj = Par1 . inj

instance Inj p a => Inj p (K1 i a x) where
  inj = K1 . inj

instance Inj p (f (g a)) => Inj p ((:.:) f g a) where
  inj = Comp1 . inj
