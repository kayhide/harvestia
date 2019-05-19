{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
module Configurable.Database.Type where

import ClassyPrelude

import Configurable (Configurable (..), fetchSetting)
import Data.Pool (Pool, createPool)
import Database.Selda.Backend (SeldaConnection, seldaClose)
import Database.Selda.PostgreSQL (pgOpen')


data Database

data DatabaseSetting = DatabaseSetting
  { host     :: Text
  , port     :: Text
  , database :: Text
  , user     :: Text
  , pool     :: Int
  }
  deriving (Eq, Show, Generic)

data DatabaseRunning = DatabaseRunning
  { pool :: Pool SeldaConnection
  }
  deriving (Generic)

instance Show DatabaseRunning where
  show (DatabaseRunning _) =
    "DatabaseRunning "
    <> "{pool = Pool {...}"
    <> "}"


instance Configurable Database where
  type Setting Database = DatabaseSetting
  type Running Database = DatabaseRunning
  type Deps Database = '[]

  ready =
    DatabaseSetting
    <$> fetchSetting "DB_HOST" "localhost"
    <*> fetchSetting "DB_PORT" "5432"
    <*> fetchSetting "DB_DATABASE" "database"
    <*> fetchSetting "DB_USER" "postgres"
    <*> fetchSetting "DB_POOL" 5

  start (DatabaseSetting host' port' database' user' pool') _ = do
    let connstr =
          "host=" <> host'
          <> " dbname=" <> database'
          <> " user=" <> user'
          <> " port=" <> port'
    pool'' <- createPool (pgOpen' Nothing (encodeUtf8 connstr)) seldaClose 4 1 pool'
    pure $ DatabaseRunning pool''
