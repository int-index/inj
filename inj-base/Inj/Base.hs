-- | 'Inj' instances for types from 'base'.

{-# LANGUAGE
    DefaultSignatures,
    FunctionalDependencies,
    FlexibleInstances,
    FlexibleContexts,
    TypeFamilies,
    ScopedTypeVariables,
    DataKinds,
    TypeOperators,
    UndecidableInstances
#-}

module Inj.Base () where

import Control.Applicative
import Control.Exception hiding (TypeError)
import Control.Monad.ST
import Data.Bifunctor
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
import Data.Ratio
import Foreign.Ptr
import GHC.Conc
import GHC.Generics
import Numeric.Natural
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec

import Inj

--------------------------------------------------------------------------------
-- Identity injections
--------------------------------------------------------------------------------

instance p ~ () => Inj p ()
instance p ~ Bool => Inj p Bool
instance p ~ Ordering => Inj p Ordering

instance p ~ Proxy t' => Inj p (Proxy t) where
  inj Proxy = Proxy

--------------------------------------------------------------------------------
-- Decision procedures for ambiguous injections
--------------------------------------------------------------------------------

data Decision_Wrap

data Decision_Map

type family DecideMaybe p where
  DecideMaybe (Maybe p) = Decision_Map
  DecideMaybe p = Decision_Wrap

class d ~ DecideMaybe p => InjMaybe d p a where
  injMaybe :: p -> Maybe a

instance InjMaybe (DecideMaybe p) p a => Inj p (Maybe a) where
  inj = injMaybe

type family DecideList p where
  DecideList [p] = Decision_Map
  DecideList p = Decision_Wrap

class d ~ DecideList p => InjList d p a where
  injList :: p -> [a]

instance InjList (DecideList p) p a => Inj p [a] where
  inj = injList

type family DecideNonEmpty p where
  DecideNonEmpty (NonEmpty p) = Decision_Map
  DecideNonEmpty p = Decision_Wrap

class d ~ DecideNonEmpty p => InjNonEmpty d p a where
  injNonEmpty :: p -> NonEmpty a

instance InjNonEmpty (DecideNonEmpty p) p a => Inj p (NonEmpty a) where
  inj = injNonEmpty

type family DecideIO p where
  DecideIO (IO p) = Decision_Map
  DecideIO p = Decision_Wrap

class d ~ DecideIO p => InjIO d p a where
  injIO :: p -> IO a

instance InjIO (DecideIO p) p a => Inj p (IO a) where
  inj = injIO

type family DecideSTM p where
  DecideSTM (STM p) = Decision_Map
  DecideSTM p = Decision_Wrap

class d ~ DecideSTM p => InjSTM d p a where
  injSTM :: p -> STM a

instance InjSTM (DecideSTM p) p a => Inj p (STM a) where
  inj = injSTM

type family DecideIdentity p where
  DecideIdentity (Identity p) = Decision_Map
  DecideIdentity p = Decision_Wrap

class d ~ DecideIdentity p => InjIdentity d p a where
  injIdentity :: p -> Identity a

instance InjIdentity (DecideIdentity p) p a => Inj p (Identity a) where
  inj = injIdentity

type family DecideZipList p where
  DecideZipList (ZipList p) = Decision_Map
  DecideZipList p = Decision_Wrap

class d ~ DecideZipList p => InjZipList d p a where
  injZipList :: p -> ZipList a

instance InjZipList (DecideZipList p) p a => Inj p (ZipList a) where
  inj = injZipList

type family DecideOption p where
  DecideOption (Option p) = Decision_Map
  DecideOption p = Decision_Wrap

class d ~ DecideOption p => InjOption d p a where
  injOption :: p -> Option a

instance InjOption (DecideOption p) p a => Inj p (Option a) where
  inj = injOption

type family DecideST p where
  DecideST (ST s p) = Decision_Map
  DecideST p = Decision_Wrap

class d ~ DecideST p => InjST d p s a where
  injST :: p -> ST s a

instance InjST (DecideST p) p s a => Inj p (ST s a) where
  inj = injST

type family DecideFn p where
  DecideFn (r -> p) = Decision_Map
  DecideFn p = Decision_Wrap

class d ~ DecideFn p => InjFn d p r a where
  injFn :: p -> r -> a

instance InjFn (DecideFn p) p r a => Inj p (r -> a) where
  inj = injFn

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

instance (Integral a, Real p) => Inj p (Ratio a) where
  inj = realToFrac

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

instance
    (DecideList p ~ Decision_Wrap, Inj p a) =>
    InjList Decision_Wrap p a
  where
    injList = pure . inj

instance
    (DecideMaybe p ~ Decision_Wrap, Inj p a) =>
    InjMaybe Decision_Wrap p a
  where
    injMaybe = pure . inj

instance
    (DecideNonEmpty p ~ Decision_Wrap, Inj p a) =>
    InjNonEmpty Decision_Wrap p a
  where
    injNonEmpty = pure . inj

instance
    (DecideIO p ~ Decision_Wrap, Inj p a) =>
    InjIO Decision_Wrap p a
  where
    injIO = pure . inj

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

instance
    (DecideSTM p ~ Decision_Wrap, Inj p a) =>
    InjSTM Decision_Wrap p a
  where
    injSTM = pure . inj

instance
    (DecideIdentity p ~ Decision_Wrap, Inj p a) =>
    InjIdentity Decision_Wrap p a
  where
    injIdentity = pure . inj

instance
    (DecideZipList p ~ Decision_Wrap, Inj p a) =>
    InjZipList Decision_Wrap p a
  where
    injZipList = pure . inj

instance
    (DecideOption p ~ Decision_Wrap, Inj p a) =>
    InjOption Decision_Wrap p a
  where
    injOption = pure . inj

instance Inj p a => Inj p (Data.Semigroup.Last a) where
  inj = pure . inj

instance Inj p a => Inj p (Data.Semigroup.First a) where
  inj = pure . inj

instance Inj p a => Inj p (Max a) where
  inj = pure . inj

instance Inj p a => Inj p (Min a) where
  inj = pure . inj

instance
    (DecideST p ~ Decision_Wrap, Inj p a) =>
    InjST Decision_Wrap p s a
  where
    injST = pure . inj

instance
    (DecideFn p ~ Decision_Wrap, Inj p a) =>
    InjFn Decision_Wrap p s a
  where
    injFn = pure . inj

instance Inj p (f (g a)) => Inj p (Compose f g a) where
  inj = Compose . inj

--------------------------------------------------------------------------------
-- 'fmap', 'bimap', etc, can be used to map injections
--------------------------------------------------------------------------------

instance
    (DecideMaybe p ~ Decision_Map, p ~ Maybe p', Inj p' a) =>
    InjMaybe Decision_Map p a
  where
    injMaybe = fmap inj

instance
    (DecideList p ~ Decision_Map, p ~ [p'], Inj p' a) =>
    InjList Decision_Map p a
  where
    injList = fmap inj

instance
    (DecideNonEmpty p ~ Decision_Map, p ~ NonEmpty p', Inj p' a) =>
    InjNonEmpty Decision_Map p a
  where
    injNonEmpty = fmap inj

instance
    (DecideIO p ~ Decision_Map, p ~ IO p', Inj p' a) =>
    InjIO Decision_Map p a
  where
    injIO = fmap inj

instance
    (DecideSTM p ~ Decision_Map, p ~ STM p', Inj p' a) =>
    InjSTM Decision_Map p a
  where
    injSTM = fmap inj

instance
    (DecideIdentity p ~ Decision_Map, p ~ Identity p', Inj p' a) =>
    InjIdentity Decision_Map p a
  where
    injIdentity = fmap inj

instance
    (DecideZipList p ~ Decision_Map, p ~ ZipList p', Inj p' a) =>
    InjZipList Decision_Map p a
  where
    injZipList = fmap inj

instance
    (DecideOption p ~ Decision_Map, p ~ Option p', Inj p' a) =>
    InjOption Decision_Map p a
  where
    injOption = fmap inj

instance
    (DecideST p ~ Decision_Map, p ~ ST s p', Inj p' a) =>
    InjST Decision_Map p s a
  where
    injST = fmap inj

instance
    (DecideFn p ~ Decision_Map, p ~ (r -> p'), Inj p' a) =>
    InjFn Decision_Map p r a
  where
    injFn = fmap inj

instance (t ~ (pa, pb), Inj pa a, Inj pb b) => Inj t (a, b) where
  inj = bimap inj inj

instance
    (t ~ (pa, pb, pc), Inj pa a, Inj pb b, Inj pc c) =>
    Inj t (a, b, c)
  where
    inj (pa, pb, pc) = (inj pa, inj pb, inj pc)

instance
    (t ~ (pa, pb, pc, pd), Inj pa a, Inj pb b, Inj pc c, Inj pd d) =>
    Inj t (a, b, c, d)
  where
    inj (pa, pb, pc, pd) = (inj pa, inj pb, inj pc, inj pd)

instance (t ~ Either pa pb, Inj pa a, Inj pb b) => Inj t (Either a b) where
  inj = bimap inj inj

instance (t ~ Const pa pb, Inj pa a) => Inj t (Const a b) where
  inj (Const pa) = Const (inj pa)

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
