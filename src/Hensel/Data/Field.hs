{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hensel.Data.Field where

import GHC.TypeLits

import Data.Proxy

infixr 8 ^^^

class Num a => Field a where
  invert :: a -> a

-- FIXME: prevent from non-prime char and too big integers.
data F (char :: Nat) = F Integer
  deriving (Eq, Show)

instance KnownNat char => Num (F char) where
  F a + F b = F ((a + b) `mod` (natVal (Proxy @char)))
  F a * F b = F ((a * b) `mod` (natVal (Proxy @char)))
  abs (F a) = F a
  signum (F a) = F a
  negate (F 0) = F 0
  negate (F a) = F ((natVal (Proxy @char)) - (a `mod` (natVal (Proxy @char))))
  fromInteger a = F (a `mod` (natVal (Proxy @char)))

(^^^) :: Field a => a -> Integer -> a
a ^^^ b = if b > 0
  then a * (a ^^^ (b-1))
  else if b == 0
    then 1
    else (invert a) ^^^ (-b)

instance KnownNat char => Field (F char) where
  invert a = a ^^^ ((natVal (Proxy @char)) - 2)
