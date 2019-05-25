module Renkon.Cli where

import ClassyPrelude

import Options.Applicative
import qualified Renkon.Command.Exec as Exec
import qualified Renkon.Command.Info as Info
import qualified Renkon.Command.List as List
import qualified Renkon.Command.Path as Path


data CommandArgs
  = ListCommand List.Args
  | InfoCommand Info.Args
  | ExecCommand Exec.Args
  | PathCommand Path.Args
  deriving (Eq, Show)


argsParser :: Parser CommandArgs
argsParser = hsubparser $
  mconcat
  [ command "list"
    ( info (ListCommand <$> List.argsParser) $
      progDesc "List available generators" )
  , command "info"
    ( info (InfoCommand <$> Info.argsParser) $
      progDesc "Display the detail information of the generator" )
  , command "exec"
    ( info (ExecCommand <$> Exec.argsParser) $
      progDesc "Execute the generator" )
  , command "path"
    ( info (PathCommand <$> Path.argsParser) $
      progDesc "Display path informations" )
  ]

optionsParser :: ParserInfo CommandArgs
optionsParser =
  info (helper <*> argsParser)
    . mconcat
    $ [fullDesc, progDesc "Generator manager"]

run :: IO ()
run = do
  args <- customExecParser (prefs showHelpOnEmpty) optionsParser
  case args of
    ListCommand args' -> List.run args'
    InfoCommand args' -> Info.run args'
    ExecCommand args' -> Exec.run args'
    PathCommand args' -> Path.run args'
