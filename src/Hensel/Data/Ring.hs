{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Hensel.Data.Ring where

import Hensel.Data.Field

import GHC.TypeLits

import Data.Proxy

class RingOver char where
  toBaseField :: R char p -> F char

data R (char :: Nat) (pow :: Nat) = R Integer
  deriving (Eq, Show)

instance (KnownNat char, KnownNat pow) => Num (R char pow) where
  R a + R b = R ((a + b) `mod` ((natVal (Proxy @char)) ^ (natVal (Proxy @pow))))
  R a * R b = R ((a * b) `mod` ((natVal (Proxy @char)) ^ (natVal (Proxy @pow))))
  abs (R a) = R a
  signum (R a) = R a
  negate (R 0) = R 0
  negate (R a) = R ( ((natVal (Proxy @char)) ^ (natVal (Proxy @pow)))
                   - (a `mod` ((natVal (Proxy @char)) ^ (natVal (Proxy @pow))))
                   )
  fromInteger a = R (a `mod` ((natVal (Proxy @char)) ^ (natVal (Proxy @pow))))

instance KnownNat char => RingOver char where
  toBaseField (R a) = F $ a `mod` (natVal (Proxy @char))
