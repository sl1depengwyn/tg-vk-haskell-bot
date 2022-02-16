module Main where

import qualified Bot

-- idk for now how to consrtuct this function [TODO]
-- getFromConfig key = do
--   maybeValue <- CFG.lookup cfg key
--   let value = case maybeValue of (Just value) -> pure value
--                                  Nothing -> undefined
--   pure value

main :: IO ()
main = Bot.main
