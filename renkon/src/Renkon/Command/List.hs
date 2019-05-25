module Renkon.Command.List
  ( Args
  , argsParser
  , run
  ) where

import ClassyPrelude hiding ((</>))

import Options.Applicative
import Renkon.Config
import Renkon.Generator
import Renkon.Util


data Args =
  Args
  { detail :: Bool
  }
  deriving (Eq, Show, Generic)

argsParser :: Parser Args
argsParser =
  Args
  <$> switch
  ( mconcat
    [ long "detail"
    , help "Show detailed informations."
    ]
  )

run :: Args -> IO ()
run (Args detail) = do
  config@PathConfig {..} <- boot
  gens' <- listGenerators config
  let displayItem = bool displayItemSimple displayItemDetail detail
  traverse_  displayItem gens'
