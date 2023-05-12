{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
module Hensel.Data.Polynomial where

import Hensel.Data.Ring
import Hensel.Data.Field

import GHC.TypeLits

-- List of coefficients starting with degree 0 without spaces.
data Poly a = Poly [a] deriving Functor

isRZero :: (Num a, Eq a) => Poly a -> Bool
isRZero (Poly a) = all (== fromInteger 0) a

instance (Num a, Eq a) => Eq (Poly a) where
  a == b = isRZero $ a - b

repr :: (Num a, Show a, Eq a) => (Int, a) -> String
repr (_, 0) = "0"
repr (0, k) = show k
repr (1, 1) = "x"
repr (1, k) = show k <> "x"
repr (deg, 1) = "x^{" <> show deg <> "}"
repr (deg, k) = show k <> "x^{" <> show deg <> "}"

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

instance (Num a, Show a, Eq a) => Show (Poly a) where
  show (Poly lst) = foldl1 (\l r -> l <> " + " <> r) (filter (/="0") $ map repr $ enumerate lst)

instance Num a => Num (Poly a) where
  Poly a + Poly b = Poly (zipWith (+) a b)
  Poly a * Poly b = Poly $ foldl1 (zipWith (+)) $
    map (\(deg, p) -> replicate deg (fromInteger 0) <> p <> replicate (length p - deg) 0) $
      enumerate $ map (\bmono -> map (* bmono) a) b
  abs = id
  signum _a = Poly [fromInteger 1]
  negate (Poly a) = Poly (map negate a)
  fromInteger a = Poly [fromInteger a]

reduce :: KnownNat char => Poly (R char pow) -> Poly (F char)
reduce = fmap toBaseField
