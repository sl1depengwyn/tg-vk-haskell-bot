module Bot.Main where

import qualified Bot.Bot             as Bot
import qualified Bot.Logger          as Logger
import qualified Data.Aeson.Extended as A
import qualified Data.Yaml           as Yaml
import qualified GHC.Generics        as G
import           System.Environment  (getArgs, getProgName)
import           System.Exit         (exitFailure)
import qualified System.IO           as IO

data Config =
  Config
    { cBot    :: Bot.Config
    , cLogger :: Logger.Config
    }
  deriving (Show, G.Generic)

instance A.FromJSON Config where
  parseJSON = A.genericParseJSON A.customOptions

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  case args of
    [configPath] -> run configPath
    _            -> run "config.yaml"

run :: FilePath -> IO ()
run path = do
  errOrConfig <- Yaml.decodeFileEither path
  Config bot logger <- either (fail . show) pure errOrConfig
  Bot.runBot bot
