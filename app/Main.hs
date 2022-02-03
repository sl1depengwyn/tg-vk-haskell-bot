module Main where

import           Bot

import qualified Data.Configurator       as CFG
import qualified Data.Configurator.Types as CFG

config :: IO CFG.Config
config = CFG.load ["config.cfg"]

-- idk for now how to consrtuct this function [TODO]
-- getFromConfig key = do
--   maybeValue <- CFG.lookup cfg key
--   let value = case maybeValue of (Just value) -> pure value
--                                  Nothing -> undefined
--   pure value
main :: IO ()
main = do
  cfg <- config
  (Just apiKey) <- CFG.lookup cfg "token"
  (Just helpMsg) <- CFG.lookup cfg "help_message"
  (Just repeatMsg) <- CFG.lookup cfg "repeat_message"
  (Just failMsg) <- CFG.lookup cfg "fail_message"
  let botConfig = BotConfig {token = apiKey, helpMessage = helpMsg, repeatMessage = repeatMsg, failMessage = failMsg}
  runBot botConfig
