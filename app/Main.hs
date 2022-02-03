module Main where

import           Bot

import qualified Data.Configurator       as CFG
import qualified Data.Configurator.Types as CFG

config :: IO CFG.Config
config = CFG.load ["config.cfg"]

main :: IO ()
main = do
  cfg <- config
  maybeApiKey <- CFG.lookup cfg "token"
  let (Just apiKey) = maybeApiKey
  let botConfig = BotConfig {token = apiKey, helpMessage = "", repeatMessage = ""}
  runBot botConfig
