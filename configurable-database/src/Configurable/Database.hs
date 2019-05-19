module Configurable.Database
  ( Database
  , run
  )
where

import ClassyPrelude

import Configurable (ToConfig, running)
import Configurable.Database.Type (Database)
import Control.Lens (view)
import Data.Extensible ((:*), Member)
import Data.Generics.Product (field')
import Data.Pool (withResource)
import Database.Selda
import Database.Selda.Backend (runSeldaT)


-- | Run SQL command
run
  :: ( Member xs Database
     , MonadReader (ToConfig :* xs) m
     , MonadIO m
     , MonadMask m
     )
  => SeldaT m a
  -> m a
run sql = do
  pool' <- view $ running @Database . field' @"pool"
  conn' <- liftIO $ withResource pool' pure
  runSeldaT sql conn'
