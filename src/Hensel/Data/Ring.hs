{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Hensel.Data.Ring where

import GHC.TypeLits

import Data.Proxy

infixr 8 ^^^

data R (char :: Nat) (pow :: Nat) = R Integer

type F (char :: Nat) = R (char :: Nat) (1 :: Nat)

class Num a => Field a where
  invert :: a -> a

class RingOver char where
  toBaseField :: R char p -> F char

instance (KnownNat char, KnownNat pow) => Eq (R char pow) where
  R a == R b =
    a `mod` ((natVal (Proxy @char)) ^ (natVal (Proxy @pow))) == b `mod` ((natVal (Proxy @char)) ^ (natVal (Proxy @pow)))

instance (KnownNat char, KnownNat pow) => Show (R char pow) where
  show (R a) = show $ a `mod` ((natVal (Proxy @char)) ^ (natVal (Proxy @pow)))

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
  toBaseField (R a) = R $ a `mod` (natVal (Proxy @char))

(^^^) :: Field a => a -> Integer -> a
a ^^^ b = if b > 0
  then a * (a ^^^ (b-1))
  else if b == 0
    then 1
    else (invert a) ^^^ (-b)

instance KnownNat char => Field (F char) where
  invert a = a ^^^ ((natVal (Proxy @char)) - 2)
