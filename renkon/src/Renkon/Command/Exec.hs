module Renkon.Command.Exec
  ( Args
  , argsParser
  , run
  ) where

import ClassyPrelude

import Options.Applicative
import Path
import Path.IO (resolveDir')
import Renkon.Config
import Renkon.Generator
import Renkon.Util


data Args =
  Args
  { generator   :: Text
  , destination :: FilePath
  , args        :: [Text]
  }
  deriving (Eq, Show, Generic)

argsParser :: Parser Args
argsParser =
  Args
  <$> strArgument
  ( mconcat
    [ metavar "GENERATOR"
    , help "Display detail information of the generator"
    ]
  )
  <*> strOption
  ( mconcat
    [ metavar "DESTINATION"
    , long "dst"
    , value "."
    , showDefault
    , help "Destination directory"
    ]
  )
  <*> some
  ( strArgument
  $ mconcat
    [ metavar "ARGS"
    , help "Generator specific arguments"
    ]
  )

-- | Run exec command.
run :: Args -> IO ()
run Args {..} = do
  config <- boot
  gen <- findGenerator config generator

  case gen of
    Nothing -> do
      withColor Red $
        say "Not found."
      withVivid White $
        say $ "  " <> generator
    Just gen' -> do
      dst' <- resolveDir' destination
      say "Launching "
      withBold Green $
        say generator
      launch gen' args


launch :: Generator -> [Text] -> IO ()
launch Generator {..} args = do
  withColor White $ do
    say $ "  exe: " <> pack (toFilePath path)
    say $ "  args: " <> tshow args
  say ""
  execute (toFilePath path) args
