module Main where

import Options.Applicative (execParser)

import Hensel.Options (options)

main :: IO ()
main = do
  _opts <- execParser options
  return ()
