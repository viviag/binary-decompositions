module Hensel.Algorithms.Hensel where

import Hensel.Data.Polynomial
import Hensel.Data.Ring

import GHC.TypeLits

-- Warning: induction on type level.

lift :: (KnownNat char, KnownNat pow) => Poly (F char) -> Poly (R char pow)
lift = undefined
