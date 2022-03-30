module Bot.Vk where

import qualified Bot.Logger                 as Logger
import qualified Bot.Database as Database
import           Control.Monad              (MonadPlus (mzero), replicateM,
                                             void)
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
import qualified GHC.Generics               as G
import           Network.HTTP.Simple
import           System.Exit                (exitFailure)
import           System.Random

apiVersion :: BC.ByteString
apiVersion = "5.131"

type MapUserToRepeat = Map Int Int

data SessionParams =
  SessionParams
    { sKey    :: T.Text
    , sServer :: T.Text
    , sTs     :: T.Text
    }
  deriving (Show)

getSessionParams :: Bot.Handle -> StateT BotState IO ()
getSessionParams botH = do
  st <- get
  let host = (Bot.hUrl . Bot.cHost . Bot.hConfig) botH
  let apiKey = (Bot.cToken . Bot.hConfig) botH
  let mbGroupId = (Bot.cGroupId . Bot.hConfig) botH
  liftIO $ when (isNothing mbGroupId) (putStrLn "No group id specified in config!" >> exitFailure)
  let (Just groupId) = mbGroupId
  let path = "/method/groups.getLongPollServer"
  let query =
        [ ("access_token", Just $ T.encodeUtf8 apiKey)
        , ("v", Just apiVersion)
        , ("group_id", Just $ T.encodeUtf8 groupId)
        ]
  sessionParams <- getResponseBody <$> httpJSON (buildRequest host path query)
  put st {sSessionParams = sessionParams}

instance A.FromJSON SessionParams where
  parseJSON =
    A.withObject
      "Bot.Vk.SessionParams"
      (\obj -> do
         o <- obj A..: "response"
         key <- o A..: "key"
         server <- o A..: "server"
         ts <- o A..: "ts"
         pure $ SessionParams {sKey = key, sServer = server, sTs = ts})

data BotState =
  BotState
    { sSessionParams :: SessionParams
    , sGen           :: StdGen
    }
  deriving (Show)

emptyState :: BotState
emptyState =
  BotState {sSessionParams = SessionParams {sKey = "", sServer = "", sTs = ""}, sGen = mkStdGen 42}

run :: Bot.Handle -> IO ()
run botH =
  evalStateT
    (do getSessionParams botH
        longPolling botH)
    emptyState

longPolling :: Bot.Handle -> StateT BotState IO ()
longPolling botH =
  forever $ do
    st <- get
    let apiKey = (Bot.cToken . Bot.hConfig) botH
    let logH = Bot.hLogger botH
    let sessionParams = sSessionParams st
    let ts = sTs sessionParams
    eitherUpdates <- getUpdates botH
    either (lift . Logger.error logH . show) (processUpdates botH) eitherUpdates

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
    { cUserId  :: UserId
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
      { uUpdateBody :: A.Object
      }
  deriving (Show, G.Generic)

instance A.FromJSON Update where
  parseJSON =
    A.withObject
      "Bot.Vk.Update"
      (\obj -> do
         type' <- obj A..: "type" :: Parser T.Text
         case type' of
           "message_new" -> UpdateWithMessage <$> ((obj A..: "object") >>= (A..: "message"))
           "message_event" -> UpdateWithCallback <$> obj A..: "object"
           unsupportedType -> pure $ UnsupportedUpdate obj)

processUpdates :: Bot.Handle -> UpdatesResponse -> StateT BotState IO ()
processUpdates botH (UpdatesResponse updates ts) = do
  st <- get
  let sessionParams = sSessionParams st
  let newState = st {sSessionParams = sessionParams {sTs = ts}}
  put newState
  last <$> mapM (processUpdate botH) updates
processUpdates botH (UpdatesErrorWithTs err ts) = do
  let logH = Bot.hLogger botH
  liftIO $ Logger.warning logH (mconcat ["Fail from vk with error code: ", show err, ", new ts: ", T.unpack ts])
  st <- get
  let sessionParams = sSessionParams st
  let newState = st {sSessionParams = sessionParams {sTs = ts}}
  put newState
processUpdates botH (UpdatesError err) = do
  let logH = Bot.hLogger botH
  liftIO $ Logger.warning logH (mconcat ["Fail from vk with error code: ", show err])
  getSessionParams botH

data UpdatesResponse
  = UpdatesResponse
      { uUpdates :: [Update]
      , uTs      :: T.Text
      }
  | UpdatesErrorWithTs
      { uFailed :: Int
      , uTs     :: T.Text
      }
  | UpdatesError
      { uFailed :: Int --error code
      }
  deriving (Show, G.Generic)

instance A.FromJSON UpdatesResponse where
  parseJSON = A.genericParseJSON A.customOptions

getUpdates :: Bot.Handle -> StateT BotState IO (Either JSONException UpdatesResponse)
getUpdates botH = do
  st <- get
  let apiKey = (Bot.cToken . Bot.hConfig) botH
  let sessionParams = sSessionParams st
  let ts = T.encodeUtf8 $ sTs sessionParams
  let key = T.encodeUtf8 $ sKey sessionParams
  let server = T.unpack $ (sServer . sSessionParams) st
  let query = [("act", Just "a_check"), ("key", Just key), ("ts", Just ts), ("wait", Just "25")]
  let request = setRequestQueryString query (parseRequest_ server)
  getResponseBody <$> httpJSONEither request

buildRequest :: BC.ByteString -> BC.ByteString -> Query -> Request
buildRequest host path query =
  setRequestHost host $
  setRequestQueryString query $ setRequestPath path $ setRequestSecure True $ setRequestPort 443 defaultRequest

data InlineKeyboard =
  InlineKeyboard
    { kIsInline :: Bool
    , kButtons  :: [[KeyboardButton]]
    }
  deriving (Show, G.Generic)

instance A.ToJSON InlineKeyboard where
  toJSON (InlineKeyboard isInline buttons) = A.object ["inline" A..= isInline, "buttons" A..= buttons]

data KeyboardButton =
  SimpleButton
    { bText :: T.Text
    , bData :: T.Text
    }
  deriving (Show)

instance A.ToJSON KeyboardButton where
  toJSON (SimpleButton buttonText buttonData) =
    A.object
      ["action" A..= A.object ["type" A..= ("callback" :: T.Text), "label" A..= buttonText, "payload" A..= buttonData]]

replyKeyboard :: InlineKeyboard
replyKeyboard =
  InlineKeyboard
    True
    [[SimpleButton "1" "1", SimpleButton "2" "2"], [SimpleButton "3" "3", SimpleButton "4" "4", SimpleButton "5" "5"]]

type ReceiverId = Int

sendMessage :: Bot.Handle -> ReceiverId -> T.Text -> Query -> StateT BotState IO ()
sendMessage botH usrId method query = do
  let host = (Bot.hUrl . Bot.cHost . Bot.hConfig) botH
  let apiKey = (Bot.cToken . Bot.hConfig) botH
  let path = mconcat ["method/", method]
  let senderId = (BC.pack . show) usrId
  st <- get
  let gen = sGen st
  let (randomId, newGen) = uniform gen :: (Int, StdGen)
  let logH = Bot.hLogger botH
  liftIO $ Logger.debug logH (mconcat ["random_id: ", show randomId])
  let queryTemplate =
        [ ("v", Just apiVersion)
        , ("access_token", Just $ T.encodeUtf8 apiKey)
        , ("user_id", Just senderId)
        , ("random_id", Just $ (BC.pack . show) randomId)
        ]
  let finalQuery = queryTemplate ++ query
  let logMessage = mconcat ["sent message by ", T.unpack method, " to ", show usrId, " with query ", show finalQuery]
  liftIO $ Logger.info logH logMessage
  httpBS (buildRequest host (T.encodeUtf8 path) finalQuery)
  put st {sGen = newGen}

sendKeyboardMessage :: Bot.Handle -> ReceiverId -> StateT BotState IO ()
sendKeyboardMessage botH usrId = sendMessage botH usrId "/messages.send" query
  where
    repeatText = (Bot.cRepeatMessage . Bot.hConfig) botH
    query = [("message", Just (T.encodeUtf8 repeatText)), ("keyboard", Just (LC.toStrict (A.encode replyKeyboard)))]

sendTextMessage :: Bot.Handle -> T.Text -> ReceiverId -> StateT BotState IO ()
sendTextMessage botH message usrId = sendMessage botH usrId "/messages.send" query
  where
    query = [("message", Just (T.encodeUtf8 message))]

sendHelpMessage :: Bot.Handle -> ReceiverId -> StateT BotState IO ()
sendHelpMessage botH = sendTextMessage botH helpText
  where
    helpText = (Bot.cHelpMessage . Bot.hConfig) botH

sendFailMessage :: Bot.Handle -> ReceiverId -> StateT BotState IO ()
sendFailMessage botH = sendTextMessage botH failText
  where
    failText = (Bot.cFailMessage . Bot.hConfig) botH

sendSticker :: Bot.Handle -> Int -> ReceiverId -> StateT BotState IO ()
sendSticker botH fileId usrId = sendMessage botH usrId "/messages.send" query
  where
    query = [("sticker_id", Just ((BC.pack . show) fileId))]

replyMessage :: Bot.Handle -> ReceiverId -> StateT BotState IO () -> StateT BotState IO ()
replyMessage botH usrId sendFunction = do
  let db = Bot.hDatabase botH
  let defNoReps = (Bot.cNumberOfResponses . Bot.hConfig) botH
  noR <- liftIO $ Database.getRepetitions db usrId
  last <$> case noR of
    [] -> replicateM defNoReps sendFunction
    noReps:_       -> replicateM noReps sendFunction

processMessage :: Bot.Handle -> Message -> StateT BotState IO ()
processMessage botH (TextMessage usrId txt) = 
  case txt of
    "/help"   -> sendHelpMessage botH usrId
    "/repeat" -> sendKeyboardMessage botH usrId
    txt'      -> replyMessage botH usrId (sendTextMessage botH txt' usrId)
processMessage botH (StickerMessage usrId fileId) = replyMessage botH usrId (sendSticker botH fileId usrId)
processMessage botH (UnsupportedMessage usrId) = sendFailMessage botH usrId

processCallback :: Bot.Handle -> CallbackQuery -> StateT BotState IO ()
processCallback botH (CallbackQuery usrId reps) = do
  let logH = Bot.hLogger botH
  liftIO $ Database.upsertUser (Bot.hDatabase botH) usrId reps
  let logMsg =
        mconcat ["update in user-repeats db: no of repeats for ", show usrId, " now is ", show reps]
  liftIO $ Logger.info logH logMsg

processFail :: Bot.Handle -> A.Object -> StateT BotState IO ()
processFail botH obj = do
  let logH = Bot.hLogger botH
  let warningMessage = mconcat ["Unsupported update: ", show obj]
  liftIO $ Logger.warning logH warningMessage

processUpdate :: Bot.Handle -> Update -> StateT BotState IO ()
processUpdate botH (UpdateWithMessage msg) = processMessage botH msg
processUpdate botH (UpdateWithCallback cb) = processCallback botH cb
processUpdate botH (UnsupportedUpdate obj) = processFail botH obj
