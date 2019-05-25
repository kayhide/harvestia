module Main where

import ClassyPrelude

import System.Environment (withArgs)
import qualified Renkon.Cli as Cli


main :: IO ()
main = do
  -- runCommand "exec --help"
  runCommand "list --detail"
  runCommand "path"
  runCommand "info sandbox"
  -- runCommand "exec monad Something"
  runCommand "exec sandbox monad Thing"



runCommand :: Text -> IO ()
runCommand args = do
  say $ "$ renkon " <> args
  withArgs (unpack <$> words args) Cli.run
  say ""
