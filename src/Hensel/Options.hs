module Hensel.Options where

import Options.Applicative

data Options = Options {
    optionsCharacteristic :: Integer
  , optionsPower :: Integer
  , optionsDegree :: Integer
  } deriving (Show, Eq)

optionsParser :: Parser Options
optionsParser = Options
  <$> option auto
      ( long "degree"
     <> short 'd'
     <> metavar "DEGREE"
     <> help "Degree of a polynomial" )
  <*> option auto
      ( long "char"
     <> short 'p'
     <> metavar "CHAR"
     <> help "Characteristic of the base field" )
  <*> option auto
      ( long "power"
     <> short 'n'
     <> metavar "POW"
     <> help "Power of characteristic" )

options :: ParserInfo Options
options = info (optionsParser <**> helper)
   ( fullDesc
  <> progDesc "Factorization of x^n-1 over Galois fields"
  <> header "hensel" )
