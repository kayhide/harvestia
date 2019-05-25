{-# LANGUAGE TemplateHaskell #-}
module Renkon.Template where

import ClassyPrelude hiding ((</>))

import Control.Monad.List
import Data.FileEmbed (embedDir)
import Path
import Path.IO
import System.FilePath.Posix (dropTrailingPathSeparator)
import Renkon.Config


type TemplateScope = Text
type TemplateName = Text

data Template =
  Template
  { path :: Path Abs Dir
  , scope :: TemplateScope
  , name :: TemplateName
  }
  deriving (Eq, Show)


listTemplates :: PathConfig -> IO [Template]
listTemplates conf = do
  xs :: [(TemplateScope, Path Abs Dir)] <- listDir' (templateBaseDir conf)
  ys :: [(TemplateScope, [(TemplateName, Path Abs Dir)])] <- traverse (traverse listDir') xs
  pure $ do
    (scope, ys') <- sort ys
    (name, path) <- sort ys'
    pure $ Template {..}

  where
    listDir' :: Path a Dir -> IO [(Text, Path Abs Dir)]
    listDir' = fmap (fmap (basename' &&& id) . fst) . listDir

    basename' :: (Path a Dir) -> Text
    basename' = pack . dropTrailingPathSeparator . toFilePath . dirname


locateTemplate :: PathConfig -> TemplateName -> IO (Maybe Template)
locateTemplate config name' = do
  templates :: [Template] <- listTemplates config
  pure $ find (\Template {..} -> name == name') templates

listTemplateFiles :: Template -> IO [(Path Rel File)]
listTemplateFiles Template { path } =
  listDirRecur path
  >>= traverse (stripProperPrefix path) . snd



templateBaseDir :: PathConfig -> Path Abs Dir
templateBaseDir (PathConfig root' _) = root' </> $(mkRelDir "templates")

initializeTemplateDirForce :: PathConfig -> IO ()
initializeTemplateDirForce conf = do
  let baseDir = templateBaseDir conf </> $(mkRelDir "renkon")
  removeDirRecur baseDir
  initializeTemplateDir conf

initializeTemplateDir :: PathConfig -> IO ()
initializeTemplateDir conf = do
  let baseDir = templateBaseDir conf </> $(mkRelDir "renkon")

  isInitialized <- doesDirExist baseDir
  unless isInitialized $ do
    traverse_ (writeData baseDir) templateData

  where
    templateData :: [(FilePath, ByteString)]
    templateData = $(embedDir "template")

    writeData :: Path Abs Dir -> (FilePath, ByteString) -> IO ()
    writeData baseDir (path, content) = do
      dst <- (baseDir </>) <$> parseRelFile path
      ensureDir $ parent dst
      writeFile (toFilePath dst) content
