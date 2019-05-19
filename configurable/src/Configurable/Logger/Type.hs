{-# LANGUAGE DeriveAnyClass #-}
module Configurable.Logger.Type where

import ClassyPrelude

import Configurable (Configurable (..), FetchSetting, fetchSetting)
import Control.Monad.Logger


data Logger

data LoggerSetting = LoggerSetting
  { stage   :: !Stage
  , verbose :: !Bool
  }
  deriving (Eq, Show, Generic)

data Stage = Development | Production | Test
  deriving (Eq, Show, Read)
  deriving FetchSetting

type LogFunc = LogLevel -> Text -> IO ()

data LoggerRunning = LoggerRunning
  { func :: !LogFunc
  }
  deriving (Generic)

instance Show LoggerRunning where
  show (LoggerRunning _) =
    "LoggerRunning "
    <> "{func = <function>"
    <> "}"


instance Configurable Logger where
  type Setting Logger = LoggerSetting
  type Running Logger = LoggerRunning
  type Deps Logger = '[]

  ready =
    LoggerSetting
    <$> fetchSetting "LOGGER_STAGE" Development
    <*> fetchSetting "LOGGER_VERBOSE" False

  start (LoggerSetting stage' verbose') _ =
    pure $ LoggerRunning
    (\level' msg' ->
        runStderrLoggingT $
        filterLogger filter' $
        logOtherN level' msg'
    )
    where
      filter' = case (verbose', stage') of
        (True, _)           -> \_ _ -> True
        (False, Production) -> \_ level'' -> LevelWarn <= level''
        (False, _)          -> \_ level'' -> LevelInfo <= level''
