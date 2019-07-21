module DevMain where

import ClassyPrelude

import qualified Main
import System.Environment (setEnv, unsetEnv)


main :: IO ()
main = do
  runCommand "LotusIpsum"


runCommand :: Text -> IO ()
runCommand args = do
  say $ "$ renkon exec renkon"
  bracket_
    do setEnv "RENKON_EXEC_ARGS" . unpack $ "exec --to-screen renkon " <> args
    do unsetEnv "RENKON_EXEC_ARGS"
    Main.main
  say ""
