module Renkon.Cli
  ( module X
  , execRenkonParser
  , inflections
  )
where

import ClassyPrelude

import Data.Aeson
import Options.Applicative as X
import qualified Renkon.Command.Exec as Exec
import Renkon.Inflector
import System.Environment (lookupEnv)


execRenkonParser :: Parser a -> IO (a, Maybe Exec.Args)
execRenkonParser parser = do
  lookupEnv "RENKON_EXEC_ARGS"
  >>= \case
    Nothing -> execBare
    Just x -> execWrapped $ words x
  where
    execWrapped args' = do
      case getParseResult (execParserPure defaultPrefs (info execArgsParser idm) args') of
        Nothing -> fail $ "Invalid exec args: " <> unwords args'
        Just execArgs@Exec.Args {..} -> do
          args'' <- handleParseResult $ execParserPure defaultPrefs (info parser idm) (unpack <$> args)
          pure (args'', Just execArgs)

    execArgsParser = subparser (command "exec" (info Exec.argsParser idm))

    execBare = do
      fmap (, Nothing) $ customExecParser (prefs showHelpOnEmpty) $ info (helper <*> parser) fullDesc


inflections :: Text -> Value
inflections x =
  object
  [ "camelcased" .= camelcase x
  , "camelcased_headed" .= camelcaseWithHead x
  , "cebabcased" .= cebabcase x
  , "snakecased" .= snakecase x
  ]
