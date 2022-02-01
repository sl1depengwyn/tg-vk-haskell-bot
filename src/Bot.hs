module Bot
  ( runBot
  ) where

import qualified Control.Monad.IO.Class     as MIO
import           Data.Aeson
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Configurator          as CFG
import qualified Data.Configurator.Types    as CFG
import           Data.Maybe
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified GHC.Generics               as G
import           Network.HTTP.Simple

type ChatID = BC.ByteString

buildRequest :: BC.ByteString -> BC.ByteString -> Query -> Request
buildRequest host path query =
  setRequestHost host $
  setRequestQueryString query $
  setRequestPath path $
  setRequestSecure True $ setRequestPort 443 defaultRequest

-- >>> buildRequest host (mconcat ["/bot", apiKey, "/sendMessage"]) [("chat_id", Nothing), ("text", Just "s")]
host :: BC.ByteString
host = "api.telegram.org"

config :: IO CFG.Config
config = CFG.load ["config.cfg"]

apiKey :: IO BC.ByteString
apiKey = do
  cfg <- config
  maybeToken <- CFG.lookup cfg "token"
  if isJust maybeToken
    then do
      let (Just token) = maybeToken
      return token
    else do
      return "No token field in config file"

sendMessage :: BC.ByteString -> Message -> IO (Response BC.ByteString)
sendMessage chatId message = do
  apiKey' <- apiKey
  let path = mconcat ["/bot", apiKey', "/sendMessage"]
  let messageText = text message
  let query =
        [("chat_id", Just chatId), ("text", Just (TE.encodeUtf8 messageText))]
  httpBS $ buildRequest host path query

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

data UpdatesResponse =
  UpdatesResponse
    { result :: [Update]
    }
  deriving (Show, G.Generic)

instance FromJSON UpdatesResponse

getUpdates :: Show a => Maybe a -> IO (Either String [Update])
getUpdates offset = do
  apiKey' <- apiKey
  let path = mconcat ["/bot", apiKey', "/getUpdates"]
  let query = [("timeout", Just "25"), ("offset", BC.pack . show <$> offset)]
  response <- httpBS $ buildRequest host path query
  let responseBody = getResponseBody response
  let responseJson =
        eitherDecode ((LC.fromChunks . return) responseBody) :: Either String UpdatesResponse
  let updates = result <$> responseJson
  return updates

replyMessage :: Update -> IO (Response BC.ByteString)
replyMessage update = sendMessage senderId (message update)
  where
    (User sndrId _) = (from . message) update
    senderId = (BC.pack . show) sndrId

longPolling :: Maybe Int -> IO ()
longPolling offset = do
  eitherUpdates <- getUpdates offset
  let (Right updates) = eitherUpdates
  let lastUpdateId =
        if null updates
          then Nothing
          else Just (updateId (last updates) + 1)
  responses <- mapM replyMessage updates
  mapM_ (BC.putStrLn . getResponseBody) responses
  longPolling lastUpdateId

runBot = longPolling Nothing
