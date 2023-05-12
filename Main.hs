{-# LANGUAGE RecordWildCards #-}
module Main where

import System.Exit (die)

import Control.Monad (when)

import Options.Applicative (execParser)

import Hensel.Options (options, Options(..))

main :: IO ()
main = do
  Options{..} <- execParser options
  return ()
