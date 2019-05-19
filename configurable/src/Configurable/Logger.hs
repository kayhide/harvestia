module Configurable.Logger
  ( Logger
  , debug
  , info
  , warn
  , error
  , logOutput
  )
where

import ClassyPrelude hiding (error)

import Configurable (ToConfig, running)
import Configurable.Logger.Type (Logger)
import Control.Lens (view)
import Control.Monad.Logger (LogLevel (..))
import Data.Extensible ((:*), Member)
import Data.Generics.Product (field')


-- | Output log text with @LogLevel@ of `Debug`.
debug
  :: ( Member xs Logger
     , MonadReader (ToConfig :* xs) m
     , MonadIO m
     )
  => Text
  -> m ()
debug msg' = logOutput LevelDebug msg'


-- | Output log text with @LogLevel@ of `Info`.
info
  :: ( Member xs Logger
     , MonadReader (ToConfig :* xs) m
     , MonadIO m
     )
  => Text
  -> m ()
info msg' = logOutput LevelInfo msg'


-- | Output log text with @LogLevel@ of `Warn`.
warn
  :: ( Member xs Logger
     , MonadReader (ToConfig :* xs) m
     , MonadIO m
     )
  => Text
  -> m ()
warn msg' = logOutput LevelWarn msg'


-- | Output log text with @LogLevel@ of `Error`.
error
  :: ( Member xs Logger
     , MonadReader (ToConfig :* xs) m
     , MonadIO m
     )
  => Text
  -> m ()
error msg' = logOutput LevelError msg'


-- | Output log text with given @LogLevel@.
logOutput
  :: ( Member xs Logger
     , MonadReader (ToConfig :* xs) m
     , MonadIO m
     )
  => LogLevel
  -> Text
  -> m ()
logOutput level' msg' = do
  func' <- view $ running @Logger . field' @"func"
  liftIO $ func' level' msg'
