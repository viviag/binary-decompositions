module Hensel.Data.Common where

repr :: (Num a, Show a, Eq a) => (Int, a) -> String
repr (_, 0) = "0"
repr (0, k) = show k
repr (1, 1) = "x"
repr (1, k) = show k <> "x"
repr (deg, 1) = "x^{" <> show deg <> "}"
repr (deg, k) = show k <> "x^{" <> show deg <> "}"

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]
