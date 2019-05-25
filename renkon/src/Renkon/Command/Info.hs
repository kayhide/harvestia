module Renkon.Command.Info
  ( Args
  , argsParser
  , run
  ) where

import ClassyPrelude

import Options.Applicative
import Renkon.Config
import Renkon.Generator
import Renkon.Util


data Args =
  Args
  { generator :: Text
  }
  deriving (Eq, Show, Generic)

argsParser :: Parser Args
argsParser =
  Args
  <$> argument str
  ( mconcat
    [ metavar "GENERATOR"
    , help "Display detail information of the generator"
    ]
  )

run :: Args -> IO ()
run (Args generator) = do
  config <- boot
  gen <- findGenerator config generator

  case gen of
    Nothing -> do
      withColor Red $
        say "generator is not found."
      withVivid White $
        say $ "  " <> generator
    Just gen' -> displayItemDetail gen'
