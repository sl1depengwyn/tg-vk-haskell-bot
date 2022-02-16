module Logger where

import qualified Data.Aeson as A
import qualified GHC.Generics               as G
import qualified Data.Text             as T

optionsJSON = A.defaultOptions  { A.fieldLabelModifier = A.camelTo2 . tail }

data Verbosity
  = Debug
  | Info
  | Warning
  | Error
  deriving (Eq, Ord, Show)

instance A.FromJSON Verbosity where
    parseJSON = A.withText "FromJSON Logger.Verbosity" $ \t ->
        case t of
            "debug"   -> pure Debug
            "info"    -> pure Info
            "warning" -> pure Warning
            "error"   -> pure Error
            _         -> fail $ "Unknown verbosity: " ++ T.unpack t

data Config =
  Config
    { cPath      :: Maybe String
    , cVerbosity :: Maybe Verbosity
    } deriving (Show)

instance A.FromJSON Config where
  parseJSON = genericParseJSON optionsJSON

newtype Handle =
  Handle
    { hConfig :: Config
    }
