module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Char (toLower)

import EDG (EDGSettings(..), parseSettings)

import qualified Examples.Simon
-- import qualified Examples.Logger

optParser :: Parser (String, EDGSettings)
optParser = (,)
  <$> argument str (
    metavar "EXAMPLE"
    <> help "The name of the example you with to run")
  <*> parseSettings

opts :: ParserInfo (String, EDGSettings)
opts = info (optParser <**> helper)
  $  fullDesc
  <> (progDesc $ "Attempt various embedded device generation problems."
    ++ "\nWith stack use 'stack exec edg-prototype -- OPTIONS'")
  <> header "edg-prototype - prototype device generation tool"

main :: IO ()
main = do
  (exampleName, settings) <- execParser opts
  case map toLower exampleName of
    "simon" -> Examples.Simon.run settings
    "big-simon" -> Examples.Simon.bigRun settings
    _ -> putStrLn "No example chosen. Available examples are 'simon','big-simon'."
