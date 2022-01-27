module Lib
    ( someFunc
    , sendMessage
    , getUpdates
    ) where



import qualified Control.Monad.IO.Class     as MIO
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Network.HTTP.Simple

someFunc :: IO ()
someFunc = putStrLn "someFunc"


type Message = T.Text
type ChatID = BC.ByteString

buildRequest :: BC.ByteString -> BC.ByteString -> Query -> Request
buildRequest host path query = setRequestHost host
                             $ setRequestQueryString query
                             $ setRequestPath path
                             $ setRequestSecure True
                             $ setRequestPort 443
                               defaultRequest

-- >>> buildRequest host (mconcat ["/bot", apiKey, "/sendMessage"]) [("chat_id", Just "385474228"), ("text", Just "s")]



host :: BC.ByteString
host = "api.telegram.org"

apiKey :: BC.ByteString
apiKey = "1438339694:AAG096Iqn-PGcd0zlBBHJ_7MwebucYdfF6M"


sendMessage :: MIO.MonadIO m => ChatID -> Message -> m (Response BC.ByteString)
sendMessage chatId message = httpBS $ buildRequest host path query
    where path = mconcat ["/bot", apiKey, "/sendMessage"]
          query = [("chat_id", Just chatId), ("text", Just (TE.encodeUtf8 message))]



getUpdates :: MIO.MonadIO m => m (Response BC.ByteString)
getUpdates = httpBS $ buildRequest host path query
    where path = mconcat ["/bot", apiKey, "/getUpdates"]
          query = [("timeout", Just "25")]
