module Renkon.Command.Path
  ( Args
  , argsParser
  , run
  ) where


import ClassyPrelude

import Options.Applicative
import Renkon.Config
import Path


data Args =
  Args
  deriving (Eq, Show, Generic)

argsParser :: Parser Args
argsParser = pure Args


run :: Args -> IO ()
run _ = do
  PathConfig base' bin' <- boot
  say $ "renkon-base: " <> pack (toFilePath base')
  say $ "renkon-bin: " <> pack (toFilePath bin')
