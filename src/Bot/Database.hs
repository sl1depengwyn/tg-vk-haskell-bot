{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Bot.Database where

import           Control.Monad              (void)
import qualified Data.Aeson.Extended        as A
import qualified Data.Pool                  as Pool
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Database.PostgreSQL.Simple as PGS
import qualified GHC.Generics               as G
import           Opaleye

newtype Config =
  Config
    { cConnectionString :: T.Text
    }
  deriving (Show, G.Generic)

instance A.FromJSON Config where
  parseJSON = A.genericParseJSON A.customOptions

data Handle =
  Handle
    { hConfig :: Config
    , hPool   :: Pool.Pool PGS.Connection
    }

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle conf f = do
  let connString = cConnectionString conf
  pool <- Pool.createPool (PGS.connectPostgreSQL $ T.encodeUtf8 connString) PGS.close 1 10 4
  let h = Handle conf pool
  initTable h
  res <- f h
  Pool.destroyAllResources pool
  return res

initTable :: Handle -> IO ()
initTable h =
  Pool.withResource (hPool h) $ \conn -> do
    void $
      PGS.execute_
        conn
        "CREATE TABLE IF NOT EXISTS users ( \
                \    id INTEGER PRIMARY KEY NOT NULL, \
                \    no_repeats INTEGER \
                \)"

data User' a b =
  User'
    { uId             :: a
    , uMessagesToSend :: b
    }
  deriving (Show)

type User = User' Int Int

type UserField = User' (Field SqlInt4) (Field SqlInt4)

$(makeAdaptorAndInstance "pUser" ''User')

usersTable :: Table UserField UserField
usersTable = table "users" (pUser User' {uId = tableField "id", uMessagesToSend = tableField "no_repeats"})

usersSelect :: Select UserField
usersSelect = selectTable usersTable

getRepetitions' :: Int -> Select (Field SqlInt4)
getRepetitions' id' = do
  user <- usersSelect
  where_ (uId user .== toFields id')
  pure $ uMessagesToSend user

getRepetitions :: Handle -> Int -> IO [Int]
getRepetitions h id' = Pool.withResource (hPool h) (\conn -> runSelect conn (getRepetitions' id'))
