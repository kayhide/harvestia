{-# LANGUAGE TemplateHaskell #-}
module Renkon.Generator where

import ClassyPrelude hiding ((</>))

import Path
import Path.IO
import Renkon.Config
import System.FilePath (getSearchPath)


type GeneratorName = Text

data Generator =
  Generator
  { path :: Path Abs File
  , name :: Text
  }
  deriving (Eq, Show)


listGenerators :: PathConfig -> IO [Generator]
listGenerators PathConfig {..} =
  getSearchPath
    >>= traverse resolveDir' . (toFilePath renkonBin :)
    >>= fmap join . traverse findRenkonGenerators


findRenkonGenerators :: Path Abs Dir -> IO [Generator]
findRenkonGenerators dir = do
  files <- filterM isExecutable =<< listDir' dir
  pure $ catMaybes $ toGenerator <$> files

  where
    listDir' :: Path Abs Dir -> IO [Path Abs File]
    listDir' dir' =
      doesDirExist dir'
      >>= bool (pure []) (snd <$> listDir dir')

    isExecutable :: Path Abs File -> IO Bool
    isExecutable file' =
      tryIOError (executable <$> getPermissions file')
      >>= \case
        Left _ -> pure False
        Right _ -> pure True


toGenerator :: Path Abs File -> Maybe Generator
toGenerator file' = Generator file' <$> toName file'
  where
    toName :: Path Abs File -> Maybe Text
    toName = stripPrefix "renkon-" . pack . toFilePath . filename


findGenerator :: PathConfig -> Text -> IO (Maybe Generator)
findGenerator conf@PathConfig {..} name' = do
  gens <- listGenerators conf
  pure $ find (\Generator {..} -> name == name') gens
