module Hensel.Algorithms.Field.Zassenhaus where

import Hensel.Data.Polynomial
import Hensel.Data.Ring

import GHC.TypeLits

factorize :: KnownNat char => Poly (F char) -> [Poly (F char)]
factorize = undefined
