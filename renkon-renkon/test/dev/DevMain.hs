module DevMain where

import ClassyPrelude

import qualified Main
import System.Environment (setEnv, unsetEnv, withArgs)
import qualified Renkon.Command as Command


main :: IO ()
main = do
  runCommandBare "LotusIpsum"
  -- runCommandWrapped "LotusIpsum"


runCommandBare :: Text -> IO ()
runCommandBare args = do
  say $ "$ renkon exec renkon"
  bracket_
    do setEnv "RENKON_EXEC_ARGS" . unpack $ "exec --to-screen renkon " <> args
    do unsetEnv "RENKON_EXEC_ARGS"
    Main.main
  say ""

-- | Wrapped version only updates after an executable is rebuilt.
runCommandWrapped :: Text -> IO ()
runCommandWrapped args = do
  say $ "$ renkon exec renkon " <> args
  withArgs (unpack <$> ["exec", "--to-screen", "renkon"] <> words args) Command.run
  say ""
