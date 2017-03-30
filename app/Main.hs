module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Char (toLower)

import EDG (EDGSettings(..), parseSettings)

import qualified Examples.Simon
import qualified NewEncoding.Simon

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
    "simon"     -> Examples.Simon.run    settings
    "med-simon" -> Examples.Simon.medRun settings
    "big-simon" -> Examples.Simon.bigRun settings
    "new-simon" -> NewEncoding.Simon.run settings
    _ -> putStrLn "No example chosen. See app/Main.hs for a list."
