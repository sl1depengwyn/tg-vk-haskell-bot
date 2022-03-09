module Bot.Bot where

import qualified Bot.Logger                 as Logger
import           Control.Monad              (MonadPlus (mzero), replicateM,
                                             void)
import qualified Control.Monad.IO.Class     as MIO
import           Control.Monad.State
import qualified Data.Aeson.Extended        as A
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as LC
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Yaml                  as Yaml
import qualified GHC.Generics               as G
import           Network.HTTP.Simple

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

type MapUserToRepeat = Map Int Int

data Handle =
  Handle
    { hConfig :: Config
    , hLogger :: Logger.Handle
    }

withHandle :: Config -> Logger.Handle -> (Handle -> IO ()) -> IO ()
withHandle conf lHandle f = f $ Handle {hConfig = conf, hLogger = lHandle}
