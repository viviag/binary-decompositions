{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Hensel.Entry where

import GHC.TypeNats

import Control.Monad (mapM_)

import Hensel.Options

import Hensel.Data.Polynomial
import Hensel.Data.Ring

import Hensel.Algorithms.Field.Zassenhaus
import Hensel.Algorithms.Hensel

import Data.Proxy

prettyPrint :: (KnownNat p, KnownNat n) => Proxy p -> Proxy n -> Poly (F p) -> [Poly (R p n)] -> IO ()
prettyPrint _ _ req resp = do
  putStrLn ("Constructed decomposition of $" <> show req <> "$ is the following:")
  putStrLn "$$"
  mapM_ (putStrLn . show) resp
  putStrLn "$$"

buildPoly :: KnownNat p => Proxy p -> Int -> Poly (F p)
buildPoly _ degree = Poly $ [-1] ++ replicate (degree - 1) 0 ++ [1]

enter :: Options -> IO ()
enter Options{..} = case (someNatVal optionsCharacteristic, someNatVal optionsPower) of
  (SomeNat p, SomeNat n) -> let poly = buildPoly p optionsDegree in
    prettyPrint p n poly (map lift $ factorize poly)
