{-# LANGUAGE TemplateHaskell #-}
module Main where

import ClassyPrelude

import Data.Aeson
import Data.FileEmbed (embedDir)
import Options.Applicative
import Renkon (execRenkon, inflections, render)


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
