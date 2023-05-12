module Main where

import System.Exit (die)

import Control.Monad (when)

import Options.Applicative (execParser)

import Hensel.Options (options)
import Hensel.Entry (enter)

main :: IO ()
main = execParser options >>= enter
