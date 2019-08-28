{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Lib where

import Control.Lens hiding (elements)
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

-- >>> [0..9]
-- [0,1,2,3,4,5,6,7,8,9]

instance KnownNat n => Finite (Fin n) where
  elements = mkFin <$> [0..(upperBound @n - 1)]

display :: Show a => String -> Iso' a a
display s = iso describe id
  where
    describe x =
      trace (s ++ "\n" ++ show x) `seq` x
