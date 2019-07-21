module Main where

import ClassyPrelude

import System.Environment (withArgs)
import qualified Renkon.Command as Command


main :: IO ()
main = do
  -- runCommand "exec --help"
  runCommand "list --detail"
  runCommand "path"



runCommand :: Text -> IO ()
runCommand args = do
  say $ "$ renkon " <> args
  withArgs (unpack <$> words args) Command.run
  say ""
