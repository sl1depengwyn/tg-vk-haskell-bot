module Bot.Bot where

import qualified Bot.Logger            as Logger
import qualified Data.Aeson.Extended   as A
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           Data.Map              (Map)
import qualified Data.Text             as T
import qualified Data.Yaml             as Yaml
import qualified GHC.Generics          as G

data Host
  = Vk
      { hUrl :: BC.ByteString
      }
  | Tg
      { hUrl :: BC.ByteString
      }
  deriving (Show)

instance A.FromJSON Host where
  parseJSON =
    A.withText "FromJSON Bot.Host" $ \t ->
      case t of
        "vk" -> pure Vk {hUrl = "api.vk.com"}
        "tg" -> pure Tg {hUrl = "api.telegram.org"}
        _    -> fail $ "Unknown host: " ++ T.unpack t

data Config =
  Config
    { cHost              :: Host
    , cToken             :: T.Text
    , cGroupId           :: Maybe T.Text
    , cHelpMessage       :: T.Text
    , cRepeatMessage     :: T.Text
    , cFailMessage       :: T.Text
    , cNumberOfResponses :: Int
    }
  deriving (Show, G.Generic)

instance A.FromJSON Config where
  parseJSON = A.genericParseJSON A.customOptions

data Handle =
  Handle
    { hConfig :: Config
    , hLogger :: Logger.Handle
    }

withHandle :: Config -> Logger.Handle -> (Handle -> IO ()) -> IO ()
withHandle conf lHandle f = f $ Handle {hConfig = conf, hLogger = lHandle}
