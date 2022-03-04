module Bot.Vk where

import qualified Bot.Logger                 as Logger
import           Control.Monad              (MonadPlus (mzero), replicateM,
                                             void)
import qualified Control.Monad.IO.Class     as MIO
import           Control.Monad.State
import qualified Data.Aeson.Extended        as A
import           Data.Aeson.Types           (Parser)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as LC
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T

import qualified Bot.Bot                    as Bot
import qualified Data.Vector                as V
import qualified GHC.Generics               as G
import           Network.HTTP.Simple

type MapUserToRepeat = Map Int Int

data BotState =
  BotState
    { sUsers  :: MapUserToRepeat
    , sTs     :: String
    , sKey    :: T.Text
    , sServer :: T.Text
    }

longPolling :: Bot.Handle -> StateT BotState IO ()
longPolling botH =
  forever $ do
    st <- get
    let apiKey = (Bot.cToken . Bot.hConfig) botH
    let ts = sTs st
    let logH = Bot.hLogger botH
    eitherUpdates <- getUpdates botH ts
    case eitherUpdates of
      (Right updatesResponse) -> do
        put st {sTs = uTs updatesResponse}
        mapM_ (processUpdate botH) (uUpdates updatesResponse)
      (Left err) -> liftIO $ Logger.error logH err

type UserId = Int

data Message
  = TextMessage
      { mFrom :: UserId
      , mText :: T.Text
      }
  | StickerMessage
      { mFrom    :: UserId
      , mSticker :: Int
      }
  | UnsupportedMessage
      { mFrom :: UserId
      }
  deriving (Show, G.Generic)

createMessage :: UserId -> T.Text -> [Maybe Int] -> Message
createMessage from "" []             = UnsupportedMessage from
createMessage from "" [Just sticker] = StickerMessage from sticker
createMessage from txt _             = TextMessage from txt

parseSticker :: A.Value -> Parser (Maybe Int)
parseSticker (A.Object obj) = do
  maybeSticker <- obj A..:? "sticker"
  case maybeSticker of
    Nothing      -> pure Nothing
    Just sticker -> sticker A..: "sticker_id"
parseSticker _ = mzero

instance A.FromJSON Message where
  parseJSON =
    A.withObject
      "Bot.Vk.Message"
      (\obj -> do
         from <- obj A..: "from_id"
         txt <- obj A..: "text"
         attachements <- obj A..: "attachments"
         parsedStickers <- mapM parseSticker attachements
         let stickers = filter isJust parsedStickers
         pure $ createMessage from txt stickers)

data CallbackQuery =
  CallbackQuery
    { cUserId :: UserId
    , cPayload :: Int
    }
  deriving (Show, G.Generic)

instance A.FromJSON CallbackQuery where
  parseJSON = A.genericParseJSON A.customOptions

data Update
  = UpdateWithMessage
      { uMessage :: Message
      }
  | UpdateWithCallback
      { uCallbackQuery :: CallbackQuery
      }
  | UnsupportedUpdate
  deriving (Show, G.Generic)

instance A.FromJSON Update where
  parseJSON = A.withObject "Bot.Vk.Update" (\obj -> do
         type' <- obj A..: "type" :: Parser T.Text
         case type' of
           "message_new"   -> UpdateWithMessage <$> ((obj A..: "object") >>= (A..: "message"))
           "message_event" -> UpdateWithCallback <$> obj A..: "object"
           unsupportedType -> pure UnsupportedUpdate)

data UpdatesResponse =
  UpdatesResponse
    { uUpdates :: [Update]
    , uTs      :: String
    }
  deriving (Show, G.Generic)

instance A.FromJSON UpdatesResponse where
  parseJSON = A.genericParseJSON A.customOptions

getUpdates :: Bot.Handle -> Maybe Int -> StateT BotState IO (Either String UpdatesResponse)
getUpdates botH offset = do
  let host = (Bot.hUrl . Bot.cHost . Bot.hConfig) botH
  let apiKey = (Bot.cToken . Bot.hConfig) botH
  let path = mconcat ["/bot", apiKey, "/getUpdates"]
  let query = [("timeout", Just "25"), ("offset", BC.pack . show <$> offset)]
  Logger.debug (Bot.hLogger botH) (T.unpack (mconcat [T.decodeUtf8 host, path]))
  response <- httpBS $ buildRequest host (T.encodeUtf8 path) query
  let responseBody = getResponseBody response
  Logger.debug (Bot.hLogger botH) ((T.unpack . T.decodeUtf8) responseBody)
  let updates = A.eitherDecodeStrict responseBody
  pure updates

buildRequest :: BC.ByteString -> BC.ByteString -> Query -> Request
buildRequest host path query =
  setRequestHost host $
  setRequestQueryString query $ setRequestPath path $ setRequestSecure True $ setRequestPort 443 defaultRequest

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

replyKeyboard :: InlineKeyboard
replyKeyboard =
  InlineKeyboard
    [[SimpleButton "1" "1", SimpleButton "2" "2"], [SimpleButton "3" "3", SimpleButton "4" "4", SimpleButton "5" "5"]]

type ReceiverId = Int

sendMessage :: Bot.Handle -> ReceiverId -> T.Text -> Query -> IO (Response BC.ByteString)
sendMessage botH usrId method query = do
  let logH = Bot.hLogger botH
  let logMessage = mconcat ["sent message by ", T.unpack method, " to ", show usrId, " with query ", show finalQuery]
  Logger.info logH logMessage
  httpBS (buildRequest host (T.encodeUtf8 path) finalQuery)
  where
    host = (Bot.hUrl . Bot.cHost . Bot.hConfig) botH
    apiKey = (Bot.cToken . Bot.hConfig) botH
    path = mconcat ["/bot", apiKey, method]
    senderId = (BC.pack . show) usrId
    chatId = ("chat_id", Just senderId)
    finalQuery = chatId : query

sendKeyboardMessage :: Bot.Handle -> ReceiverId -> IO (Response BC.ByteString)
sendKeyboardMessage botH usrId = sendMessage botH usrId "/sendMessage" query
  where
    repeatText = (Bot.cRepeatMessage . Bot.hConfig) botH
    query = [("text", Just (T.encodeUtf8 repeatText)), ("reply_markup", Just (LC.toStrict (A.encode replyKeyboard)))]

sendTextMessage :: Bot.Handle -> T.Text -> ReceiverId -> IO (Response BC.ByteString)
sendTextMessage botH message usrId = sendMessage botH usrId "/sendMessage" query
  where
    query = [("text", Just (T.encodeUtf8 message))]

sendHelpMessage :: Bot.Handle -> ReceiverId -> IO (Response BC.ByteString)
sendHelpMessage botH = sendTextMessage botH helpText
  where
    helpText = (Bot.cHelpMessage . Bot.hConfig) botH

sendFailMessage :: Bot.Handle -> ReceiverId -> IO (Response BC.ByteString)
sendFailMessage botH = sendTextMessage botH failText
  where
    failText = (Bot.cFailMessage . Bot.hConfig) botH

sendSticker :: Bot.Handle -> Int -> ReceiverId -> IO (Response BC.ByteString)
sendSticker botH fileId usrId = sendMessage botH usrId "/sendSticker" query
  where
    query = [("sticker", Just ((BC.pack . show) fileId))]

replyMessage :: Bot.Handle -> ReceiverId -> IO (Response BC.ByteString) -> StateT BotState IO [Response BC.ByteString]
replyMessage botH usrId sendFunction = do
  st <- get
  let defNoReps = (Bot.cNumberOfResponses . Bot.hConfig) botH
  case Map.lookup usrId (sUsers st) of
    (Just noReps) -> liftIO $ replicateM noReps sendFunction
    Nothing       -> liftIO $ replicateM defNoReps sendFunction

processMessage :: Bot.Handle -> Message -> StateT BotState IO ()
processMessage botH (TextMessage usrId txt) = do
  case txt of
    "/help" -> void $ liftIO $ sendHelpMessage botH usrId
    "/repeat" -> void $ liftIO $ sendKeyboardMessage botH usrId
    txt' -> void $ replyMessage botH usrId (sendTextMessage botH txt' usrId)
  return ()
processMessage botH (StickerMessage usrId fileId) = void $ replyMessage botH usrId (sendSticker botH fileId usrId)
processMessage botH (UnsupportedMessage usrId) = liftIO $ void (sendFailMessage botH usrId)

processCallback :: Bot.Handle -> CallbackQuery -> StateT BotState IO ()
processCallback botH (CallbackQuery usrId reps) = do
  st <- get
  let usersToReps = sUsers st
  let logH = Bot.hLogger botH
  let newMap = Map.insert usrId reps usersToReps
  let logMsg =
        mconcat ["update in user-repeats map: for ", show usrId, " ", show $ Map.lookup usrId usersToReps, " ➔ ", show reps]
  liftIO $ Logger.info logH logMsg
  let newState = st {sUsers = newMap}
  put newState

processUpdate :: Bot.Handle -> Update -> StateT BotState IO ()
processUpdate botH (UpdateWithMessage msg) = processMessage botH msg
processUpdate botH (UpdateWithCallback cb) = processCallback botH cb
