{-# LANGUAGE TemplateHaskell #-}

module Renkon.Config where

import ClassyPrelude hiding ((</>))

import GHC.Generics (Generic)
import Path (Abs, Dir, Path, mkRelDir, parseAbsDir, toFilePath, (</>))
import Path.IO (ensureDir, getHomeDir)
import System.Environment (lookupEnv)


data PathConfig = PathConfig
  { renkonRoot   :: Path Abs Dir
  , renkonBin    :: Path Abs Dir
  }
  deriving (Show, Generic)

-- * Booting

boot :: IO PathConfig
boot = do
  config@(PathConfig root' _) <- setupPathConfig
  ensureDir root'
  pure config


-- * Internal functions

renkonBaseDir :: IO (Path Abs Dir)
renkonBaseDir = do
  r <- lookupEnv "RENKON_BASE_DIR"
  case r of
    Nothing   -> (</> $(mkRelDir ".renkon")) <$> getHomeDir
    Just path -> parseAbsDir path


setupPathConfig :: IO PathConfig
setupPathConfig = do
  root' <- renkonBaseDir
  pure $ PathConfig root' (root' </> $(mkRelDir "bin"))
