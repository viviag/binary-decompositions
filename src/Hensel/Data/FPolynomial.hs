{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hensel.Data.FPolynomial where

import Hensel.Data.Common
import Hensel.Data.Field

import GHC.TypeLits

-- List of coefficients starting with degree 0 without spaces.
data FPoly (char :: Nat) = FPoly [F char]

isFZero :: (KnownNat char) => FPoly char -> Bool
isFZero (FPoly a) = all (== fromInteger 0) a

instance (KnownNat char) => Eq (FPoly char) where
  a == b = isFZero $ a - b

instance (KnownNat char) => Show (FPoly char) where
  show (FPoly lst) = foldl1 (\l r -> l <> " + " <> r) (filter (/="0") $ map repr $ enumerate lst)

instance (KnownNat char) => Num (FPoly char) where
  FPoly a + FPoly b = FPoly (zipWith (+) a b)
  FPoly a * FPoly b = FPoly $ foldl1 (zipWith (+)) $
    map (\(deg, p) -> replicate deg (fromInteger 0) <> p <> replicate (length p - deg) 0) $
      enumerate $ map (\bmono -> map (* bmono) a) b
  abs = id
  signum _a = FPoly [fromInteger 1]
  negate (FPoly a) = FPoly (map negate a)
  fromInteger a = FPoly [fromInteger a]
