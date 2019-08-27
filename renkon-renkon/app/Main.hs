{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import ClassyPrelude

import Renkon
import Renkon.Cli


argsParser :: Parser Value
argsParser = do
  name' <- strArgument
    ( mconcat
      [ metavar "NAME"
      , help "Name"
      ]
    )
  pure $ object
    [ "name" .= inflections name'
    ]


templateData :: [(FilePath, ByteString)]
templateData = $(embedDir "template")


main :: IO ()
main =
  execRenkon argsParser $
    traverse_ render templateData
