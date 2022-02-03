module Bot
  ( runBot
  , BotConfig(..)
  ) where

import qualified Control.Monad.IO.Class     as MIO
import           Data.Aeson
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as LC
import           Data.Maybe
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified GHC.Generics               as G
import           Network.HTTP.Simple

type ChatID = BC.ByteString

data BotConfig =
  BotConfig
    { token         :: BC.ByteString
    , helpMessage   :: T.Text
    , repeatMessage :: T.Text
    }

buildRequest :: BC.ByteString -> BC.ByteString -> Query -> Request
buildRequest host path query =
  setRequestHost host $
  setRequestQueryString query $ setRequestPath path $ setRequestSecure True $ setRequestPort 443 defaultRequest

-- >>> buildRequest host (mconcat ["/bot", apiKey, "/sendMessage"]) [("chat_id", Nothing), ("text", Just "s")]
host :: BC.ByteString
host = "api.telegram.org"

data User =
  User
    { id    :: Int
    , isBot :: Bool
    }
  deriving (Show, G.Generic)

instance FromJSON User where
  parseJSON (Object o) = User <$> o .: "id" <*> o .: "is_bot"

data Message =
  Message
    { from :: User
    , text :: T.Text
    }
  deriving (Show, G.Generic)

instance FromJSON Message

data Update =
  Update
    { updateId :: Int
    , message  :: Message
    }
  deriving (Show)

instance FromJSON Update where
  parseJSON (Object o) = Update <$> o .: "update_id" <*> o .: "message"

newtype UpdatesResponse =
  UpdatesResponse
    { result :: [Update]
    }
  deriving (Show, G.Generic)

instance FromJSON UpdatesResponse

sendMessage :: BC.ByteString -> BC.ByteString -> Message -> IO (Response BC.ByteString)
sendMessage apiKey chatId message = httpBS $ buildRequest host path query
  where
    path = mconcat ["/bot", apiKey, "/sendMessage"]
    messageText = text message
    query = [("chat_id", Just chatId), ("text", Just (TE.encodeUtf8 messageText))]

getUpdates :: Show a => BC.ByteString -> Maybe a -> IO (Either String [Update])
getUpdates apiKey offset = do
  let path = mconcat ["/bot", apiKey, "/getUpdates"]
  let query = [("timeout", Just "25"), ("offset", BC.pack . show <$> offset)]
  response <- httpBS $ buildRequest host path query
  let responseBody = getResponseBody response
  let responseJson = eitherDecode ((LC.fromChunks . return) responseBody) :: Either String UpdatesResponse
  let updates = result <$> responseJson
  return updates

replyTextMessage :: BC.ByteString -> Update -> IO (Response BC.ByteString)
replyTextMessage apiKey update = sendMessage apiKey senderId (message update)
  where
    (User sndrId _) = (from . message) update
    senderId = (BC.pack . show) sndrId

processTextMessage = undefined

longPolling :: BotConfig -> Maybe Int -> IO ()
longPolling config offset = do
  let (BotConfig apiKey _ _) = config
  eitherUpdates <- getUpdates apiKey offset
  let (Right updates) = eitherUpdates
  let lastUpdateId =
        if null updates
          then Nothing
          else Just (updateId (last updates) + 1)
  responses <- mapM (replyTextMessage apiKey) updates
  mapM_ (BC.putStrLn . getResponseBody) responses
  longPolling config lastUpdateId

runBot :: BotConfig -> IO ()
runBot config = longPolling config Nothing
