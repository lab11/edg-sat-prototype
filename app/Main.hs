module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Char (toLower)

import EDG (EDGSettings(..), parseSettings)

import qualified Examples.Simon
import qualified NewEncoding.Blinky
import qualified NewEncoding.Simon
import qualified NewEncoding.Datalogger
import qualified NewEncoding.FeedbackFan
import qualified NewEncoding.Robot
import qualified NewEncoding.SimonTrinket
import qualified NewEncoding.AlternativeSimonTrinket

import qualified Experiments

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
    "new-blinky" -> NewEncoding.Blinky.run settings
    "new-min-blinky" -> NewEncoding.Blinky.minRun settings
    "new-simon" -> NewEncoding.Simon.run settings
    "new-min-simon" -> NewEncoding.Simon.minRun settings
    "new-datalogger" -> NewEncoding.Datalogger.run settings
    "new-min-datalogger" -> NewEncoding.Datalogger.minRun settings
    "new-feedbackfan" -> NewEncoding.FeedbackFan.run settings
    "new-min-feedbackfan" -> NewEncoding.FeedbackFan.minRun settings
    "new-robot" -> NewEncoding.Robot.run settings
    "new-min-robot" -> NewEncoding.Robot.minRun settings
    "new-simon-trinket" -> NewEncoding.SimonTrinket.run settings
    "new-min-simon-trinket" -> NewEncoding.SimonTrinket.minRun settings
    "new-alt-simon-trinket" -> NewEncoding.AlternativeSimonTrinket.run settings
    "new-min-alt-simon-trinket" -> NewEncoding.AlternativeSimonTrinket.minRun settings
    "random" -> Experiments.run settings
    _ -> putStrLn "No example chosen. See app/Main.hs for a list."
