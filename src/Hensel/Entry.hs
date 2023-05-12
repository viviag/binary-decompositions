{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Hensel.Entry where

import GHC.TypeLits

import Control.Monad (mapM_)

import Hensel.Options

import Hensel.Data.Polynomial
import Hensel.Data.Ring

import Hensel.Algorithms.Field.Zassenhaus
import Hensel.Algorithms.Hensel

import Data.Proxy

prettyPrint :: Poly (F 2) -> [Poly (R 2 2)] -> IO ()
prettyPrint req resp = do
  putStrLn ("Constructed decomposition of $" <> show req <> "$ is the following:")
  putStrLn "$$"
  mapM_ (putStrLn . show) resp
  putStrLn "$$"

buildPoly :: Int -> Poly (F 2)
buildPoly degree = Poly $ [-1] ++ replicate (degree - 1) 0 ++ [1]

enter :: Options -> IO ()
enter Options{..} = prettyPrint poly (map lift $ factorize poly)
  where poly = buildPoly optionsDegree
