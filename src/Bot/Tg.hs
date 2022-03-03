module Bot.Tg where

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
import qualified Bot.Bot as Bot

type MapUserToRepeat = Map Int Int

data BotState =
  BotState
    { sUsers  :: MapUserToRepeat
    , sOffset :: Maybe Int
    }

runBot :: Bot.Handle -> IO ()
runBot botH = evalStateT (longPolling botH) (BotState {sUsers = Map.empty, sOffset = Nothing})

longPolling :: Bot.Handle -> StateT BotState IO ()
longPolling botH =
  forever $ do
    st <- get
    let apiKey = (Bot.cToken . Bot.hConfig) botH
    let offset = sOffset st
    let logH = Bot.hLogger botH
    eitherUpdates <- liftIO $ getUpdates botH offset
    case eitherUpdates of
      (Right updates) -> do
        unless (null updates) (put st {sOffset = Just (uUpdateId (last updates) + 1)})
        mapM_ (processUpdate botH) updates
      (Left err) -> liftIO $ Logger.error logH err

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

instance A.FromJSON Sticker where
  parseJSON = A.genericParseJSON A.customOptions

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

getUpdates :: Bot.Handle -> Maybe Int -> IO (Either String [Update])
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
  pure $ result <$> updates

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

sendSticker :: Bot.Handle -> T.Text -> ReceiverId -> IO (Response BC.ByteString)
sendSticker botH fileId usrId = sendMessage botH usrId "/sendSticker" query
  where
    query = [("sticker", Just (T.encodeUtf8 fileId))]

replyMessage :: Bot.Handle -> ReceiverId -> IO (Response BC.ByteString) -> StateT BotState IO [Response BC.ByteString]
replyMessage botH usrId sendFunction = do
  st <- get
  let defNoReps = (Bot.cNumberOfResponses . Bot.hConfig) botH
  case Map.lookup usrId (sUsers st) of
    (Just noReps) -> liftIO $ replicateM noReps sendFunction
    Nothing       -> liftIO $ replicateM defNoReps sendFunction

processMessage :: Bot.Handle -> Message -> StateT BotState IO ()
processMessage botH (TextMessage us txt) = do
  let usrId = uId us
  case txt of
    "/help" -> void $ liftIO $ sendHelpMessage botH usrId
    "/repeat" -> void $ liftIO $ sendKeyboardMessage botH usrId
    txt' -> void $ replyMessage botH usrId (sendTextMessage botH txt' usrId)
  return ()
processMessage botH (StickerMessage us st) =
  let usrId = uId us
      fileId = sFileId st
   in void $ replyMessage botH usrId (sendSticker botH fileId usrId)
processMessage botH (UnsupportedMessage us) =
  let usrId = uId us
   in liftIO $ void (sendFailMessage botH usrId)

processCallback :: Bot.Handle -> CallbackQuery -> StateT BotState IO ()
processCallback botH (CallbackQuery (User usrId) reps) = do
  st <- get
  let usersToReps = sUsers st
  let logH = Bot.hLogger botH
  let newMap = Map.insert usrId (read reps) usersToReps
  let logMsg =
        mconcat ["update in user-repeats map: for ", show usrId, " ", show $ Map.lookup usrId usersToReps, " ➔ ", reps]
  liftIO $ Logger.info logH logMsg
  let newState = st {sUsers = newMap}
  put newState

processUpdate :: Bot.Handle -> Update -> StateT BotState IO ()
processUpdate botH (UpdateWithMessage _ msg) = processMessage botH msg
processUpdate botH (UpdateWithCallback _ cb) = processCallback botH cb
