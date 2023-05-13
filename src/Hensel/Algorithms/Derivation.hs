module Hensel.Algorithms.Derivation where

import Hensel.Data.Polynomial

d :: Num a => Poly a -> Poly a
d (Poly (free:coeffs)) = Poly . map (\(a,b) -> fromInteger a * b) $ zip [1..] coeffs
