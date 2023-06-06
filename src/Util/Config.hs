module Util.Config (appConfig, pgConfig, AppConfig (..)) where

import Data.Functor ((<&>))
import Database.PostgreSQL.Simple (ConnectInfo (..))
import System.Environment (getEnv)

newtype AppConfig = AppConfig {port :: Int} deriving (Show)

-- TODO EVERYTHING --

appConfig :: IO AppConfig
appConfig = do
  port <- getEnv "PORT" <&> read
  return AppConfig {port = port}

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
