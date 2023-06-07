module Util.Config (appConfig, pgConfig, AppConfig (..)) where

import Data.Functor ((<&>))
import qualified Data.Text as T
import Database.PostgreSQL.Simple (ConnectInfo (..))
import System.Environment (getEnv)

data AppConfig = AppConfig
  { port :: Int,
    s3BucketName :: T.Text,
    s3Region :: T.Text
  }
  deriving (Show)

-- TODO EVERYTHING --

appConfig :: IO AppConfig
appConfig = do
  port <- getEnv "PORT" <&> read
  s3BucketName <- getEnv "BUCKET_NAME"
  s3Region <- getEnv "AWS_DEFAULT_REGION"
  return
    AppConfig
      { port = port,
        s3BucketName = T.pack s3BucketName,
        s3Region = T.pack s3Region
      }

pgConfig :: IO ConnectInfo
pgConfig = do
  host <- getEnv "PG_HOST"
  port <- getEnv "PG_PORT" <&> read
  user <- getEnv "PG_USER"
  pass <- getEnv "PG_PASS"
  db <- getEnv "PG_DB"
  return
    ConnectInfo
      { connectHost = host,
        connectPort = port,
        connectUser = user,
        connectPassword = pass,
        connectDatabase = db
      }
