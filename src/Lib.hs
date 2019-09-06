{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Lib where

import Control.Lens hiding (elements)
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Debug.Trace
import GHC.TypeNats
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

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

unFin :: Fin n -> Int
unFin (Fin i) = i

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
      trace (s ++ "\n" ++ show x ++ "\nsize: " ++ show @Int (ceiling (sizeOf x))) x

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

decToRat :: Decimal -> Rational
decToRat (Decimal x n) = (fromIntegral x)*(10 ^^ (- fromIntegral n))















nines :: [Fin 10]
nines = replicate 100 9

-- >>> show nines
-- "[9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9]"

cycles :: [Fin 10]
cycles = take 100 $ cycle [0,1,2,3,4,5,6,7,8,9]

-- >>> show cycles
-- "[0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9]"

-- Related to Entropy in information theory
-- Kolmogorov complexity

runLengthEncoding :: Eq a => Iso' [a] [(a,Int)]
runLengthEncoding = iso encode decode
  where
    encode l = map (\as -> (head as, length as)) $ group l
    decode l = concatMap (\(a,n) -> replicate n a) l

instance (Sizeable a, Sizeable b) => Sizeable (a, b) where
  sizeOf (a, b) = sizeOf a + sizeOf b

algo1 :: Sizeable a => Show a => Eq a => Iso' [a] [(a,Int)]
algo1 = display "INPUT" . from (display "OUTPUT") . runLengthEncoding . display "COMPRESSED"

roundTrip :: Iso' a b -> a -> ()
roundTrip f a = (view (from f) $ view f a) `seq` ()

-- >>> roundTrip algo1 nines
-- INPUT
-- [9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9]
-- size: 333
-- COMPRESSED
-- [(9,100)]
-- size: 10
-- OUTPUT
-- [9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9]
-- size: 333
-- ()

-- >>> roundTrip algo1 cycles
-- INPUT
-- [0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9]
-- size: 333
-- COMPRESSED
-- [(0,1),(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1),(9,1),(0,1),(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1),(9,1),(0,1),(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1),(9,1),(0,1),(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1),(9,1),(0,1),(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1),(9,1),(0,1),(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1),(9,1),(0,1),(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1),(9,1),(0,1),(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1),(9,1),(0,1),(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1),(9,1),(0,1),(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1),(9,1)]
-- size: 433
-- OUTPUT
-- [0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9]
-- size: 333
-- ()


-- >>> sort cycles
-- [0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,9]


-- >>> roundTrip algo1 $ sort cycles
-- INPUT
-- [0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,9]
-- size: 333
-- COMPRESSED
-- [(0,10),(1,10),(2,10),(3,10),(4,10),(5,10),(6,10),(7,10),(8,10),(9,10)]
-- size: 67
-- OUTPUT
-- [0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,9]
-- size: 333
-- ()

-- The algorithm we're implementing is based on the bzip2 algorithm.

data WithEos a
  = Symb a
  | EOS
  deriving (Ord, Eq)

instance Finite a => Finite (WithEos a) where
  elements = map Symb (elements @a) ++[EOS]

instance Sizeable a => Sizeable (WithEos a) where
  sizeOf (Symb x) = 1 + sizeOf x
  sizeOf EOS = 1

instance Show a => Show (WithEos a) where
  show (Symb x) = show x
  show EOS = "^"

cyclicRotations :: [a] -> [[a]]
cyclicRotations as =
  [ r ++ l | n <- [0..length as - 1], let (l,r) = splitAt n as ]

-- >>> cyclicRotations "Banana"
-- ["Banana","ananaB","nanaBa","anaBan","naBana","aBanan"]

-- >>> cyclicRotations "Arnaud"
-- ["Arnaud","rnaudA","naudAr","audArn","udArna","dArnau"]

-- >>> sort $ cyclicRotations "Arnaud"
-- ["Arnaud","audArn","dArnau","naudAr","rnaudA","udArna"]

-- >>> transpose $ sort $ cyclicRotations "Arnaud"
-- ["Aadnru","ruAand","ndruaA","aAndur","uraAdn","dnurAa"]

-- >>> transpose $ sort $ cyclicRotations "Mississipi River"
-- [" MReiiiiiprssssv","Riir pssviMiisse","isvMRisse ipsiir","vseii iirRsispsM","eirsvRpsMis iisi","rsMseiisiviRp is","Msiirv isesiiRps","iissMeRpsrsv iii","spssiriiiMieRv s","siiisMv siprieRs","i spsieRssiMvrii","sRsiisriis ieMvp","sii ssMvpiRsriei","ivpRsiieisisMsr ","peiiissr sviisMR","ir vpssMRiessiii"]

bwt :: Ord a => Iso' [a] [WithEos a]
bwt = iso encode decode
  where
    encode as = last $ transpose $ sort $ cyclicRotations (map Symb as ++ [EOS])
    decode w = decodeLoop w (length w) (map (const []) w)

    decodeLoop w n st =
      if n == 0 then
        extract st
      else
        decodeLoop w (n-1) $ sort $ zipWith (:) w st

    extract st = mapMaybe unEOS $ fromJust $ find (\w -> last w == EOS) st

    unEOS (Symb b) = Just b
    unEOS EOS = Nothing

algo2 :: Ord a => Sizeable a => Show a => Eq a => Iso' [a] [(WithEos a,Int)]
algo2 = display "INPUT" . from (display "OUTPUT") . bwt . display "BWT" . runLengthEncoding . display "COMPRESSED"

-- >>> roundTrip algo2 cycles
-- INPUT
-- [0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9]
-- size: 333
-- BWT
-- [^,9,9,9,9,9,9,9,9,9,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,8,8,9]
-- size: 434
-- COMPRESSED
-- [(^,1),(9,9),(0,10),(1,10),(2,10),(3,10),(4,10),(5,10),(6,10),(7,10),(8,10),(9,1)]
-- size: 84
-- OUTPUT
-- [0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9]
-- size: 333
-- ()

rollerCoaster :: [Fin 10]
rollerCoaster = take 100 $ cycle [0,1,2,3,4,5,6,7,8,9,8,7,6,5,4,3,2,1]

-- >>> rollerCoaster
-- [0,1,2,3,4,5,6,7,8,9,8,7,6,5,4,3,2,1,0,1,2,3,4,5,6,7,8,9,8,7,6,5,4,3,2,1,0,1,2,3,4,5,6,7,8,9,8,7,6,5,4,3,2,1,0,1,2,3,4,5,6,7,8,9,8,7,6,5,4,3,2,1,0,1,2,3,4,5,6,7,8,9,8,7,6,5,4,3,2,1,0,1,2,3,4,5,6,7,8,9]

-- >>> roundTrip algo2 rollerCoaster
-- INPUT
-- [0,1,2,3,4,5,6,7,8,9,8,7,6,5,4,3,2,1,0,1,2,3,4,5,6,7,8,9,8,7,6,5,4,3,2,1,0,1,2,3,4,5,6,7,8,9,8,7,6,5,4,3,2,1,0,1,2,3,4,5,6,7,8,9,8,7,6,5,4,3,2,1,0,1,2,3,4,5,6,7,8,9,8,7,6,5,4,3,2,1,0,1,2,3,4,5,6,7,8,9]
-- size: 333
-- BWT
-- [^,1,1,1,1,1,2,2,2,2,2,0,0,0,0,0,0,3,3,3,3,3,1,1,1,1,1,1,4,4,4,4,4,2,2,2,2,2,2,5,5,5,5,5,3,3,3,3,3,3,6,6,6,6,6,4,4,4,4,4,4,7,7,7,7,7,5,5,5,5,5,5,8,8,8,8,8,6,6,6,6,6,6,9,9,9,9,9,7,7,7,7,7,7,8,8,8,8,8,8,9]
-- size: 434
-- COMPRESSED
-- [(^,1),(1,5),(2,5),(0,6),(3,5),(1,6),(4,5),(2,6),(5,5),(3,6),(6,5),(4,6),(7,5),(5,6),(8,5),(6,6),(9,5),(7,6),(8,6),(9,1)]
-- size: 130
-- OUTPUT
-- [0,1,2,3,4,5,6,7,8,9,8,7,6,5,4,3,2,1,0,1,2,3,4,5,6,7,8,9,8,7,6,5,4,3,2,1,0,1,2,3,4,5,6,7,8,9,8,7,6,5,4,3,2,1,0,1,2,3,4,5,6,7,8,9,8,7,6,5,4,3,2,1,0,1,2,3,4,5,6,7,8,9,8,7,6,5,4,3,2,1,0,1,2,3,4,5,6,7,8,9]
-- size: 333
-- ()

mississipis :: [Fin 10]
mississipis = take 100 $ cycle [0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5]

-- >>> mississipis
-- [0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2]

-- >>> roundTrip algo2 mississipis
-- INPUT
-- [0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2]
-- size: 333
-- BWT
-- [^,5,5,5,5,5,5,0,0,0,0,0,0,2,2,2,2,2,2,0,2,2,2,2,2,2,3,3,3,3,3,3,5,5,5,5,5,5,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,7,7,7,7,7,7,4,4,4,4,4,4,1,1,1,1,1,1,6,6,6,6,6,6,2]
-- size: 434
-- COMPRESSED
-- [(^,1),(5,6),(0,6),(2,6),(0,1),(2,6),(3,6),(5,6),(2,12),(1,13),(2,1),(1,12),(7,6),(4,6),(1,6),(6,6),(2,1)]
-- size: 111
-- OUTPUT
-- [0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2]
-- size: 333
-- ()

moveToTop0 :: forall a. Finite a => Iso' [a] [Int]
moveToTop0 = iso encode decode
  where
    encode :: [a] -> [Int]
    encode as = flip evalState (elements @a) $
      forM as $ \a -> do
        stack <- get
        put $ moveUp a stack
        return $ fromJust $ elemIndex a stack

    decode :: [Int] -> [a]
    decode is = flip evalState (elements @a) $
      forM is $ \i -> do
        stack <- get
        let a = stack !! i
        put $ moveUp a stack
        return a

    moveUp a as =
      a : filter (/= a) as

algo3 :: Finite a => Sizeable a => Show a => Iso' [a] [(Int, Int)]
algo3 = display "INPUT" . from (display "OUTPUT") . bwt . display "BWT" . moveToTop0 . display "MoveToTop" . runLengthEncoding . display "COMPRESSED"

-- >>> roundTrip algo3 mississipis
-- INPUT
-- [0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2]
-- size: 333
-- BWT
-- [^,5,5,5,5,5,5,0,0,0,0,0,0,2,2,2,2,2,2,0,2,2,2,2,2,2,3,3,3,3,3,3,5,5,5,5,5,5,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,7,7,7,7,7,7,4,4,4,4,4,4,1,1,1,1,1,1,6,6,6,6,6,6,2]
-- size: 434
-- MoveToTop
-- [10,6,0,0,0,0,0,2,0,0,0,0,0,4,0,0,0,0,0,1,1,0,0,0,0,0,5,0,0,0,0,0,3,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,7,0,0,0,0,0,2,0,0,0,0,0,8,0,0,0,0,0,4]
-- size: 116
-- COMPRESSED
-- [(10,1),(6,1),(0,5),(2,1),(0,5),(4,1),(0,5),(1,2),(0,5),(5,1),(0,5),(3,1),(0,5),(2,1),(0,11),(5,1),(0,12),(1,2),(0,11),(8,1),(0,5),(7,1),(0,5),(2,1),(0,5),(8,1),(0,5),(4,1)]
-- size: 92
-- OUTPUT
-- [0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2]
-- size: 333
-- ()

moveToTop :: forall n a. KnownNat n => Finite a => Iso' [a] [Fin n]
moveToTop = moveToTop0 . iso (map (mkFin @n)) (map (unFin @n))

algo3' :: KnownNat n => Finite a => Sizeable a => Show a => Iso' [a] [(Fin n, Int)]
algo3' = display "INPUT" . from (display "OUTPUT") . bwt . display "BWT" . moveToTop . display "MoveToTop" . runLengthEncoding . display "COMPRESSED"

-- >>> roundTrip (algo3' @11) mississipis
-- <interactive>:702:13-22: error:
--     Pattern syntax in expression context: algo3'@11
--     Did you mean to enable TypeApplications?

-- 1, 10, 11, 100, 101, 110, 111
-- 001, 10, 00011, 100, 101, 110, 111

-- 1, 2, 11, 12, 21, 22, 111

data WithRuns (n :: Nat)
  = I (Fin n)
  | One
  | Two
  deriving (Eq, Ord)

instance KnownNat n => Finite (WithRuns n) where
  elements = map I (elements @(Fin n)) ++ [One, Two]

deriving via (Symbols (WithRuns n)) instance KnownNat n => Sizeable (WithRuns n)

instance Show (WithRuns n) where
  show (I i) = show i
  show One = "R1"
  show Two = "R2"

intToRun :: Int -> [WithRuns n]
intToRun 0 = []
intToRun ((`divMod` 2) -> (n2, 0)) = Two : intToRun (n2 - 1)
intToRun ((`divMod` 2) -> (n2, _)) = One : intToRun n2

runToInt :: [WithRuns n] -> Int
runToInt [] = 0
runToInt (One : rs) = 1 + 2 * (runToInt rs)
runToInt (Two : rs) = 2 + 2 * (runToInt rs)
runToInt _ = error "I can't do this"

isDigit :: WithRuns n -> Bool
isDigit One = True
isDigit Two = True
isDigit _ = False

-- >> intToRun 5

runLengthEncoding2 :: KnownNat n => Iso' [Fin n] [WithRuns n]
runLengthEncoding2 = runLengthEncoding . iso unpack pack
  where
    unpack = concatMap (\(i, n) -> if i == 0 then intToRun n else replicate n (I i))
    pack rs = map packOne $ groupBy (\r1 r2 -> isDigit r1 && isDigit r2) rs

    packOne [I i] = (i, 1)
    packOne rs = (0, runToInt rs)

algo3'' :: KnownNat n => Finite a => Sizeable a => Show a => Iso' [a] [WithRuns n]
algo3'' = display "INPUT" . from (display "OUTPUT") . bwt . display "BWT" . moveToTop . display "MoveToTop" . runLengthEncoding2 . display "COMPRESSED"

algo3_10 ::Iso' [Fin 10] [WithRuns 11]
algo3_10 = algo3''

-- >>> roundTrip algo3_10 mississipis
-- INPUT
-- [0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2]
-- size: 333
-- BWT
-- [^,5,5,5,5,5,5,0,0,0,0,0,0,2,2,2,2,2,2,0,2,2,2,2,2,2,3,3,3,3,3,3,5,5,5,5,5,5,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,7,7,7,7,7,7,4,4,4,4,4,4,1,1,1,1,1,1,6,6,6,6,6,6,2]
-- size: 434
-- MoveToTop
-- [10,6,0,0,0,0,0,2,0,0,0,0,0,4,0,0,0,0,0,1,1,0,0,0,0,0,5,0,0,0,0,0,3,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,7,0,0,0,0,0,2,0,0,0,0,0,8,0,0,0,0,0,4]
-- size: 350
-- COMPRESSED
-- [10,6,R1,R2,2,R1,R2,4,R1,R2,1,1,R1,R2,5,R1,R2,3,R1,R2,2,R1,R1,R2,5,R2,R1,R2,1,1,R1,R1,R2,8,R1,R2,7,R1,R2,2,R1,R2,8,R1,R2,4]
-- size: 171
-- OUTPUT
-- [0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2,1,2,2,1,3,1,4,5,1,6,7,5,0,1,2,2]
-- size: 333
-- ()

-- Interval |- - - - - - - - - -|- - - - -|- - - - -|
--          |- - - - -|- - -|- - -|

type Model s = Map s Int

newModel :: forall s. Finite s => Model s
newModel = Map.fromList $ map (\s -> (s, 1)) (elements @s)

updateModel :: Ord s => Model s -> s -> Model s
updateModel m s = Map.adjust (+1) s m

boundaries :: Ord s => Model s -> [(s, Rational)]
boundaries m =
  let
    (symbs, qs) = unzip $ Map.toAscList m
    bs = tail $ scanl (+) 0 (map (\q -> (fromIntegral q) / total) qs)
  in
    zip symbs bs
  where
    total = fromIntegral $ sum $ m

intervals :: Ord s => Model s -> Map s Interval
intervals m =
  let
    bs = boundaries m
    shifted = 0:(map snd bs)
  in
    Map.fromList $ zipWith (\(s, r) l -> (s, Interval l r)) bs shifted


-- for arithmetic coding see also: https://patternsinfp.wordpress.com/2017/12/05/arithmetic-coding/

data Interval = Interval Rational Rational

unitInterval :: Interval
unitInterval = Interval 0 1

contains :: Interval -> Rational -> Bool
contains (Interval l r) x = l <= x && x < r

narrow :: Interval -> Interval -> Interval
narrow i (Interval p q) = Interval (weight i p) (weight i q)

weight :: Interval -> Rational -> Rational
weight (Interval l r) x = l + (r-l)*x

widen :: Interval -> Interval -> Interval
widen i (Interval p q) = Interval (scale i p) (scale i q)

scale :: Interval -> Rational -> Rational
scale (Interval l r) x = (x - l)/(r - l)

arithmeticEncode :: forall s. Finite s => Iso' [s] Decimal
arithmeticEncode = iso encode decode
  where
    encode ss =
      let (Interval l r) = encodeMany (addEos ss) in
        findDecimalBetween l r
    decode x = remEos $ decodeInfty (decToRat x)

    encodeMany :: [WithEos s] -> Interval
    encodeMany ss = snd $ flip execState (newModel, unitInterval) $
      forM ss encodeOne

    encodeOne :: WithEos s -> State (Model (WithEos s), Interval) ()
    encodeOne s = do
      (m, i0) <- get
      let i = encodeSym m s
      put (updateModel m s, narrow i0 i)

    decodeInfty :: Rational -> [WithEos s]
    decodeInfty x = unfoldr decodeOne (newModel, x)

    decodeOne :: (Model (WithEos s), Rational) -> Maybe ((WithEos s), (Model (WithEos s), Rational))
    decodeOne (m, x) =
      let s = decodeSym m x in
        Just (s, (updateModel m s, scale (encodeSym m s) x))


    encodeSym :: Model (WithEos s) -> (WithEos s) -> Interval
    encodeSym m s = fromJust $ Map.lookup s (intervals m)

    decodeSym :: Model (WithEos s) -> Rational -> (WithEos s)
    decodeSym m x =
      fst $ fromJust $ find (\(_,b) -> x < b)$ boundaries m

    addEos xs = map Symb xs ++ [EOS]

    remEos xs = map (\(Symb s) -> s)$ takeWhile (/= EOS) xs


algo4 :: Iso' [Fin 10] Decimal
algo4 = display "INPUT" . from (display "OUTPUT") . bwt . display "BWT" . moveToTop @11 . display "MoveToTop" . runLengthEncoding2 . display "COMPRESSED" . arithmeticEncode . display "ARITHMETIC ENCODED"

-- >>> roundTrip algo4 mississipis
