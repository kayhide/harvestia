module Renkon.Renderer where

import ClassyPrelude hiding ((</>))

import Data.Aeson (Value)
import Data.FileEmbed
import Formatting
import Options.Applicative
import Path
import Renkon.Config
import Renkon.Template
import Renkon.Util
import Text.Mustache (compileMustacheFile, compileMustacheText, renderMustache)


render :: Template -> Path Abs Dir -> Value -> IO ()
render template'@Template { path } dst binding = do
  templateFiles <- listTemplateFiles template'
  xs <- traverse (process binding path dst) templateFiles
  traverse_ print' xs

  where
    print' :: (Path Rel File, Text) -> IO ()
    print' (file', content') = do
      say ""
      withColor Cyan $ do
        say . pack $ toFilePath file'
      withVivid Black $ do
        say content'


process
  :: Value
  -> Path Abs Dir               -- ^ Source directory
  -> Path Abs Dir               -- ^ Destination directory
  -> Path Rel File              -- ^ File path to be interpolated
  -> IO (Path Rel File, Text)   -- ^ File path and its content filled with the binding
process binding src dst path = do
  let path' = toFilePath path
  pathTemp <- case compileMustacheText "path" $ pack path' of
    Left err    -> fail $ show err
    Right temp' -> pure temp'
  let path'' = renderMustache pathTemp binding
  path''' <- parseRelFile (unpack path'')

  contentTemp <- compileMustacheFile $ toFilePath $ src </> path
  let content' = renderMustache contentTemp binding

  pure $ (path''', toStrict content')
