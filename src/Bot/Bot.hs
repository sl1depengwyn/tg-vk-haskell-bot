module Bot.Bot where

import qualified Bot.Logger                 as Logger
import           Control.Monad              (MonadPlus (mzero), replicateM)
import qualified Control.Monad.IO.Class     as MIO
import qualified Data.Aeson.Extended        as A
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as LC
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Yaml                  as Yaml
import qualified GHC.Generics               as G
import           Network.HTTP.Simple

data Config =
  Config
    { cToken             :: T.Text
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

type ChatID = BC.ByteString

type UserToRepeat = Map Int Int

host :: BC.ByteString
host = "api.telegram.org"

buildRequest :: BC.ByteString -> BC.ByteString -> Query -> Request
buildRequest host path query =
  setRequestHost host $
  setRequestQueryString query $ setRequestPath path $ setRequestSecure True $ setRequestPort 443 defaultRequest

replyKeyboard :: InlineKeyboard
replyKeyboard =
  InlineKeyboard
    [[SimpleButton "1" "1", SimpleButton "2" "2"], [SimpleButton "3" "3", SimpleButton "4" "4", SimpleButton "5" "5"]]

sendMessage :: T.Text -> Int -> T.Text -> Query -> IO BC.ByteString
sendMessage apiKey userId method query = getResponseBody <$> httpBS (buildRequest host (TE.encodeUtf8 path) finalQuery)
  where
    path = mconcat ["/bot", apiKey, method]
    senderId = (BC.pack . show) userId
    chatId = ("chat_id", Just senderId)
    finalQuery = chatId : query

sendKeyboardMessage :: T.Text -> Int -> T.Text -> IO BC.ByteString
sendKeyboardMessage apiKey userId repeatText = sendMessage apiKey userId "/sendMessage" query
  where
    query = [("text", Just (TE.encodeUtf8 repeatText)), ("reply_markup", Just (LC.toStrict (A.encode replyKeyboard)))]

sendTextMessage :: T.Text -> T.Text -> Int -> IO BC.ByteString
sendTextMessage apiKey message userId = sendMessage apiKey userId "/sendMessage" query
  where
    query = [("text", Just (TE.encodeUtf8 message))]

sendSticker :: T.Text -> T.Text -> Int -> IO BC.ByteString
sendSticker apiKey fileId userId = sendMessage apiKey userId "/sendSticker" query
  where
    query = [("sticker", Just (TE.encodeUtf8 fileId))]

newtype User =
  User
    { uId :: Int
    }
  deriving (Show, G.Generic)

instance A.FromJSON User where
  parseJSON = A.genericParseJSON A.customOptions

newtype Sticker =
  Sticker
    { sFileId :: T.Text
    }
  deriving (Show, G.Generic)

instance A.FromJSON Sticker

newtype InlineKeyboard =
  InlineKeyboard
    { inline_keyboard :: [[KeyboardButton]]
    }
  deriving (Show, G.Generic)

instance A.ToJSON InlineKeyboard

data KeyboardButton =
  SimpleButton
    { bText :: T.Text
    , bData :: T.Text
    }
  deriving (Show)

instance A.ToJSON KeyboardButton where
  toJSON (SimpleButton buttonText buttonData) = A.object ["text" A..= buttonText, "callback_data" A..= buttonData]

data Message
  = TextMessage
      { mFrom :: User
      , mText :: T.Text
      }
  | StickerMessage
      { mFrom    :: User
      , mSticker :: Sticker
      }
  | UnsupportedMessage
      { mFrom :: User
      }
  deriving (Show, G.Generic)

instance A.FromJSON Message where
  parseJSON = A.genericParseJSON A.customOptions

data CallbackQuery =
  CallbackQuery
    { cFrom :: User
    , cData :: String
    }
  deriving (Show, G.Generic)

instance A.FromJSON CallbackQuery where
  parseJSON = A.genericParseJSON A.customOptions

data Update
  = UpdateWithMessage
      { uUpdateId :: Int
      , uMessage  :: Message
      }
  | UpdateWithCallback
      { uUpdateId      :: Int
      , uCallbackQuery :: CallbackQuery
      }
  deriving (Show, G.Generic)

instance A.FromJSON Update where
  parseJSON = A.genericParseJSON A.customOptions

newtype UpdatesResponse =
  UpdatesResponse
    { result :: [Update]
    }
  deriving (Show, G.Generic)

instance A.FromJSON UpdatesResponse

getUpdates :: Show a => T.Text -> Maybe a -> IO (Either String [Update])
getUpdates apiKey offset = do
  let path = mconcat ["/bot", apiKey, "/getUpdates"]
  let query = [("timeout", Just "25"), ("offset", BC.pack . show <$> offset)]
  response <- httpBS $ buildRequest host (TE.encodeUtf8 path) query
  let responseBody = getResponseBody response
  let responseJson = A.eitherDecodeStrict responseBody :: Either String UpdatesResponse
  let updates = result <$> responseJson
  return updates

replyTextMessage :: T.Text -> UserToRepeat -> Message -> IO BC.ByteString
replyTextMessage apiKey usersDB msg = sendTextMessage apiKey (mText msg) senderId
  where
    (User senderId) = mFrom msg

replyMessage :: UserToRepeat -> Int -> Int -> (Int -> IO BC.ByteString) -> IO BC.ByteString
replyMessage usersDB noRepetitions userId f =
  case Map.lookup userId usersDB of
    (Just noReps) -> mconcat <$> replicateM noReps (f userId)
    Nothing       -> mconcat <$> replicateM noRepetitions (f userId)

processMessage :: Config -> UserToRepeat -> Message -> IO BC.ByteString
processMessage (Config api helpMsg repeatMsg _ noRepetitions) usersDB msg@(TextMessage (User userId) text) =
  case text of
    "/help" -> sendTextMessage api helpMsg userId
    "/repeat" -> sendKeyboardMessage api userId repeatMsg
    _ -> replyMessage usersDB noRepetitions userId (sendTextMessage api text)
processMessage (Config api _ _ _ noRepetitions) usersDB (StickerMessage (User userId) (Sticker fileId)) =
  replyMessage usersDB noRepetitions userId (sendSticker api fileId)
processMessage (Config api _ _ failMsg _) usersDB (UnsupportedMessage (User userId)) =
  sendTextMessage api failMsg userId

processCallback :: UserToRepeat -> CallbackQuery -> UserToRepeat
processCallback usersDB (CallbackQuery (User usrId) amount) = Map.insert usrId (read amount) usersDB

processUpdates :: Config -> UserToRepeat -> [Update] -> [(UserToRepeat, IO (Maybe BC.ByteString))]
processUpdates config usersDB ((UpdateWithMessage _ msg):others) =
  (usersDB, Just <$> processMessage config usersDB msg) : processUpdates config usersDB others
processUpdates config usersDB ((UpdateWithCallback _ callback):others) =
  (processCallback usersDB callback, pure Nothing) : processUpdates config usersDB others
processUpdates config usersDB [] = []

longPolling :: Config -> UserToRepeat -> Maybe Int -> IO ()
longPolling config usersDB offset = do
  let (Config apiKey _ _ _ _) = config
  eitherUpdates <- getUpdates apiKey offset
  let (Right updates) = eitherUpdates
  let lastUpdateId =
        if null updates
          then Nothing
          else Just (uUpdateId (last updates) + 1)
  let responses = processUpdates config usersDB updates
  let upToDateDB =
        if null responses
          then usersDB
          else fst $ last responses
  let responseBodiesIO = mapM snd responses
  responseBodies <- responseBodiesIO
  mapM_ (maybe (BC.putStrLn "map updated") BC.putStrLn) responseBodies
  longPolling config upToDateDB lastUpdateId

runBot :: Config -> IO ()
runBot config = longPolling config Map.empty Nothing
