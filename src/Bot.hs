module Bot
  ( runBot
  , BotConfig(..)
  , Message(..)
  ) where

import           Control.Monad              (MonadPlus (mzero))
import qualified Control.Monad.IO.Class     as MIO
import           Data.Aeson
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Map                   as M
import           Data.Maybe
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified GHC.Generics               as G
import           Network.HTTP.Simple

type ChatID = BC.ByteString

type UserToRepeat = M.Map Int Int

data BotConfig =
  BotConfig
    { token         :: BC.ByteString
    , helpMessage   :: T.Text
    , repeatMessage :: T.Text
    , failMessage   :: T.Text
    }

buildRequest :: BC.ByteString -> BC.ByteString -> Query -> Request
buildRequest host path query =
  setRequestHost host $
  setRequestQueryString query $ setRequestPath path $ setRequestSecure True $ setRequestPort 443 defaultRequest

-- >>> buildRequest host (mconcat ["/bot", apiKey, "/sendTextMessage"]) [("chat_id", Nothing), ("text", Just "s")]
host :: BC.ByteString
host = "api.telegram.org"

usersDB :: UserToRepeat
usersDB = M.Map.fromList []

newtype User =
  User
    { id :: Int
    }
  deriving (Show, G.Generic)

instance FromJSON User

newtype Sticker =
  Sticker
    { file_id :: T.Text
    }
  deriving (Show, G.Generic)

instance FromJSON Sticker

data Message
  = TextMessage
      { from :: User
      , text :: T.Text
      }
  | StickerMessage
      { from    :: User
      , sticker :: Sticker
      }
  | UnsupportedMessage
      { from :: User
      }
  deriving (Show, G.Generic)

instance FromJSON Message where
  parseJSON = genericParseJSON (defaultOptions {sumEncoding = UntaggedValue})

data Update =
  Update
    { updateId :: Int
    , message  :: Message
    }
  deriving (Show)

instance FromJSON Update where
  parseJSON (Object o) = Update <$> o .: "update_id" <*> o .: "message"
  parseJSON _          = mzero

newtype UpdatesResponse =
  UpdatesResponse
    { result :: [Update]
    }
  deriving (Show, G.Generic)

instance FromJSON UpdatesResponse

sendTextMessage :: BC.ByteString -> Int -> T.Text -> IO (Response BC.ByteString)
sendTextMessage apiKey userId message = httpBS $ buildRequest host path query
  where
    path = mconcat ["/bot", apiKey, "/sendMessage"]
    senderId = (BC.pack . show) userId
    query = [("chat_id", Just senderId), ("text", Just (TE.encodeUtf8 message))]

sendSticker :: (Show a) => BC.ByteString -> a -> T.Text -> IO (Response BC.ByteString)
sendSticker apiKey userId fileId = httpBS $ buildRequest host path query
  where
    path = mconcat ["/bot", apiKey, "/sendSticker"]
    senderId = (BC.pack . show) userId
    query = [("chat_id", Just senderId), ("sticker", Just (TE.encodeUtf8 fileId))]

getUpdates :: Show a => BC.ByteString -> Maybe a -> IO (Either String [Update])
getUpdates apiKey offset = do
  let path = mconcat ["/bot", apiKey, "/getUpdates"]
  let query = [("timeout", Just "25"), ("offset", BC.pack . show <$> offset)]
  response <- httpBS $ buildRequest host path query
  let responseBody = getResponseBody response
  let responseJson = eitherDecode ((LC.fromChunks . return) responseBody) :: Either String UpdatesResponse
  let updates = result <$> responseJson
  return updates

replyTextMessage :: BC.ByteString -> Message -> IO (Response BC.ByteString)
replyTextMessage apiKey msg = sendTextMessage apiKey senderId (text msg)
  where
    (User senderId) = from msg

processMessage :: BotConfig -> Message -> IO (Response BC.ByteString)
processMessage (BotConfig api helpMsg repeatMsg _) (TextMessage (User userId) text) =
  case text of
    "/help"   -> sendTextMessage api userId helpMsg
    "/repeat" -> sendTextMessage api userId repeatMsg
    _         -> sendTextMessage api userId text
processMessage (BotConfig api _ _ _) (StickerMessage (User userId) (Sticker fileId)) = sendSticker api userId fileId
processMessage (BotConfig api _ _ failMsg) (UnsupportedMessage (User userId)) = sendTextMessage api userId failMsg

longPolling :: BotConfig -> Maybe Int -> IO ()
longPolling config offset = do
  let (BotConfig apiKey _ _ _) = config
  eitherUpdates <- getUpdates apiKey offset
  let (Right updates) = eitherUpdates
  let lastUpdateId =
        if null updates
          then Nothing
          else Just (updateId (last updates) + 1)
  let messages = map message updates
  responses <- mapM (processMessage config) messages
  mapM_ (BC.putStrLn . getResponseBody) responses
  longPolling config lastUpdateId

runBot :: BotConfig -> IO ()
runBot config = longPolling config Nothing
