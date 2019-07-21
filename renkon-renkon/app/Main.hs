{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import ClassyPrelude

import Renkon
import Renkon.Cli
import qualified Renkon.Command.Exec as Exec


data Args = Args
  { name :: Text
  }
  deriving (Eq, Show)

argsParser :: Parser Args
argsParser =
  Args
  <$> strArgument
  ( mconcat
    [ metavar "NAME"
    , help "Name"
    ]
  )


toBinding :: Args -> Value
toBinding Args {..} =
  object
  [ "name" .= inflections name
  ]


templateData :: [(FilePath, ByteString)]
templateData = $(embedDir "template")


main :: IO ()
main = do
  (args', execArgs) <- execRenkonParser argsParser
  let toScreen = maybe False Exec.toScreen execArgs
  let render' = bool renderToFile renderToScreen toScreen
  traverse_ (uncurry $ render' (toBinding args')) templateData
