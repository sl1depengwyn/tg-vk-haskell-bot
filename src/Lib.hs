module Lib
    ( someFunc
    , sendMessage
    , getUpdates
    , runBot
    ) where



import qualified Control.Monad.IO.Class     as MIO
import           Data.Aeson
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified GHC.Generics as G
import           Network.HTTP.Simple




someFunc :: IO ()
someFunc = putStrLn "someFunc"


type ChatID = BC.ByteString

buildRequest :: BC.ByteString -> BC.ByteString -> Query -> Request
buildRequest host path query = setRequestHost host
                             $ setRequestQueryString query
                             $ setRequestPath path
                             $ setRequestSecure True
                             $ setRequestPort 443
                               defaultRequest

-- >>> buildRequest host (mconcat ["/bot", apiKey, "/sendMessage"]) [("chat_id", Nothing), ("text", Just "s")]




host :: BC.ByteString
host = "api.telegram.org"

apiKey :: BC.ByteString
apiKey = "<Your API Key>"


sendMessage :: MIO.MonadIO m => ChatID -> Message -> m (Response BC.ByteString)
sendMessage chatId message = httpBS $ buildRequest host path query
    where path = mconcat ["/bot", apiKey, "/sendMessage"]
          messageText = text message
          query = [("chat_id", Just chatId), ("text", Just (TE.encodeUtf8 messageText))]



data User = User { id    :: Int
                 , isBot :: Bool } deriving (Show, G.Generic)
instance FromJSON User where
    parseJSON (Object o) = User <$> o .: "id"
                                <*> o .: "is_bot"

data Message = Message { from :: User
                       , text :: T.Text } deriving (Show, G.Generic)
instance FromJSON Message

data Update = Update { updateId :: Int
                     , message  :: Message } deriving (Show)

instance FromJSON Update where
    parseJSON (Object o) = Update <$> o .: "update_id"
                                  <*> o .: "message"

data UpdatesResponse = UpdatesResponse { result :: [Update] } deriving (Show, G.Generic)
instance FromJSON UpdatesResponse

getUpdates :: (MIO.MonadIO m) => Maybe Int -> m (Either String [Update])
getUpdates offset = do
        let path = mconcat ["/bot", apiKey, "/getUpdates"]
        let query = [("timeout", Just "25"), ("offset", BC.pack . show <$> offset)]
        response <- httpBS $ buildRequest host path query
        let responseBody = getResponseBody response
        let responseJson = eitherDecode ((LC.fromChunks . return) responseBody) :: Either String UpdatesResponse
        let updates = result <$> responseJson
        return updates


replyMessage update = sendMessage senderId (message update)
    where (User sndrId _) = (from . message) update
          senderId = (BC.pack . show) sndrId
         



runBot :: IO ()
runBot = do
    updatesM <- getUpdates Nothing
    let (Right updates) = updatesM
    mapM_ replyMessage updates




