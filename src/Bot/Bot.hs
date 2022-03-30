module Bot.Bot where

import qualified Bot.Database as Database
import qualified Bot.Logger            as Logger
import qualified Data.Aeson.Extended   as A
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           Data.Map              (Map)
import qualified Data.Text             as T
import qualified Data.Yaml             as Yaml
import qualified GHC.Generics          as G

-- small remark:
-- you can see different approaches in vk and tg parts
-- in vk i avoid any redundant data types for parsing from json however
-- in tg part i use some "ad-hoc" data types to use generic parsing and i have implemented 
-- minimum amount of parsing by hands
-- idk which is the best, seems like tg is more scalable, but vk is little more clear and clean

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
    , hDatabase :: Database.Handle
    , hLogger :: Logger.Handle
    }

withHandle :: Config -> Database.Handle -> Logger.Handle -> (Handle -> IO ()) -> IO ()
withHandle conf dbHandle lHandle f = f $ Handle {hConfig = conf, hDatabase = dbHandle, hLogger = lHandle}
