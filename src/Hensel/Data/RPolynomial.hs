{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hensel.Data.RPolynomial where

import Hensel.Data.Common
import Hensel.Data.Ring
import Hensel.Data.FPolynomial

import GHC.TypeLits

-- List of coefficients starting with degree 0 without spaces.
data RPoly (char :: Nat) (pow :: Nat) = RPoly [R char pow]

isRZero :: (KnownNat char, KnownNat pow) => RPoly char pow -> Bool
isRZero (RPoly a) = all (== fromInteger 0) a

instance (KnownNat char, KnownNat pow) => Eq (RPoly char pow) where
  a == b = isRZero $ a - b

instance (KnownNat char, KnownNat pow) => Show (RPoly char pow) where
  show (RPoly lst) = foldl1 (\l r -> l <> " + " <> r) (filter (/="0") $ map repr $ enumerate lst)

instance (KnownNat char, KnownNat pow) => Num (RPoly char pow) where
  RPoly a + RPoly b = RPoly (zipWith (+) a b)
  RPoly a * RPoly b = RPoly $ foldl1 (zipWith (+)) $
    map (\(deg, p) -> replicate deg (fromInteger 0) <> p <> replicate (length p - deg) 0) $
      enumerate $ map (\bmono -> map (* bmono) a) b
  abs = id
  signum _a = RPoly [fromInteger 1]
  negate (RPoly a) = RPoly (map negate a)
  fromInteger a = RPoly [fromInteger a]

reduce :: KnownNat char => RPoly char pow -> FPoly char
reduce (RPoly lst) = FPoly $ map toBaseField lst
