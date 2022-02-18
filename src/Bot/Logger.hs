module Bot.Logger where

import qualified Data.Aeson.Extended as A
import qualified GHC.Generics               as G
import qualified Data.Text             as T

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
    } deriving (Show, G.Generic)

instance A.FromJSON Config where
  parseJSON = A.genericParseJSON A.customOptions 
  
newtype Handle =
  Handle
    { hConfig :: Config
    }
