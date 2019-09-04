{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Lib where

import Control.Lens hiding (elements)
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Debug.Trace
import GHC.TypeNats

-- There is a more interesting definition of finite types which is to require
-- that (a->) is traversable. While it's more interesting, it doesn't seem
-- particularly helpful here, so I'm keeping to the simple definition.
class Ord a => Finite a where
  elements :: [a]

instance (Finite a, Finite b) => Finite (a, b) where
  elements = [ (a, b) | a <- elements @a, b <- elements @b ]

-- | Integers from 0 (included) to @n@ (excluded).
newtype Fin (n :: Nat) = Fin Int
  deriving (Eq, Ord)

upperBound :: forall n. KnownNat n => Int
upperBound = fromIntegral (natVal (Proxy @n))

mkFin :: forall n. KnownNat n => Int -> Fin n
mkFin i =
  if i < upperBound @n then Fin i
  else error $ "Incorrect bounded number value: " ++ show i ++ " >= " ++ show (upperBound @n)

instance Show (Fin n) where
  show (Fin i) = show i

instance KnownNat n => Num (Fin n) where
  fromInteger = mkFin . fromInteger
  (+) = error "Not implemented"
  (*) = error "Not implemented"
  abs = error "Not implemented"
  signum = error "Not implemented"
  negate = error "Not implemented"

-- >>> [0..9]
-- [0,1,2,3,4,5,6,7,8,9]

instance KnownNat n => Finite (Fin n) where
  elements = mkFin <$> [0..(upperBound @n - 1)]

class Sizeable a where
  sizeOf :: a -> Double

instance Sizeable a => Sizeable [a] where
  sizeOf as = getSum $ foldMap (Sum . sizeOf @a) as

newtype Symbols a = Symbols a

-- >>> logBase 2 8
-- 3.0

-- >>> logBase 2 6
-- 2.584962500721156

instance Finite a => Sizeable (Symbols a) where
  sizeOf (Symbols _) = logBase 2 (fromIntegral (length (elements @a)))

deriving via (Symbols (Fin n)) instance KnownNat n => Sizeable (Fin n)

instance Sizeable Int where
  sizeOf i = logBase 2 (max (fromIntegral i) 2)

instance Sizeable Integer where
  sizeOf i = logBase 2 (max (fromIntegral i) 2)

display :: (Show a, Sizeable a) => String -> Iso' a a
display s = iso describe id
  where
    describe x =
      trace (s ++ "\n" ++ show x ++ "\nsize: " ++ show @Int (ceiling (sizeOf x))) `seq` x

-- x*10^-n , assumed smaller than one
data Decimal = Decimal Integer Int

instance Show Decimal where
  show (Decimal x n) = "0." ++ digits x n
    where
      digits y p =
        if y >= 10 ^ (p-1) then show y
        else "0" ++ digits y (p-1)

instance Sizeable Decimal where
  sizeOf (Decimal _ n) = (fromIntegral n) * logBase 2 10

-- Assumes that the difference between the two inputs is less than one
findDecimalBetween :: Rational -> Rational -> Decimal
findDecimalBetween lo hi = Decimal (ceiling (lo/(10 ^^ (-powerOfTen)))) powerOfTen
  where
    powerOfTen = findPowerOfTenBelow (hi - lo)

    findPowerOfTenBelow :: Rational -> Int
    findPowerOfTenBelow bound = fromJust $ find (\n -> 10^^(-n) < bound) [0..]

-- >>> findDecimalBetween (1/2) (3/4)
-- 0.5

-- >>> findDecimalBetween (1/3) (1/2)
-- 0.4

-- >>> findDecimalBetween (1/12345) (2/12345)
-- 0.00009

-- >>> findDecimalBetween (123/12345) (124/12345)
-- 0.00997
