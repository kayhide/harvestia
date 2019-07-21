module Renkon.Renderer where

import ClassyPrelude hiding ((</>))

import Control.Monad.Catch.Pure (runCatch)
import Data.Aeson (Value)
import Path
import Path.IO (ensureDir)
import Renkon.Util
import Text.Mustache (compileMustacheText, renderMustache)


process
  :: MonadIO m
  => Value                   -- ^ Binding injecting to the template
  -> FilePath                -- ^ Source directory
  -> ByteString              -- ^ File path to be interpolated
  -> m (Path Rel File, Text) -- ^ File path and its content filled with the binding
process binding path' body' = do
  pathTemp <- either throwIO pure $ compileMustacheText "path" $ pack path'
  path'' <- either throwIO pure $ runCatch $
    parseRelFile $ unpack $ renderMustache pathTemp binding

  bodyTemp <- either throwIO pure $ compileMustacheText "body" $ decodeUtf8 body'
  let body'' = renderMustache bodyTemp binding

  pure (path'', toStrict body'')


renderToFile
  :: forall m.
     (MonadIO m, MonadUnliftIO m)
  => Value
  -> FilePath
  -> ByteString
  -> m ()
renderToFile binding path' body'  = do
  (path'', body'') <- process binding path' body'
  withColor Yellow $
    say . pack $ toFilePath path''
  ensureDir $ parent path''
  writeFileUtf8 (toFilePath path'') body''

renderToScreen
  :: forall m.
     (MonadIO m, MonadUnliftIO m)
  => Value
  -> FilePath
  -> ByteString
  -> m ()
renderToScreen binding path' body'  = do
  data' <- process binding path' body'
  print' data'
  where
    print' :: (Path Rel File, Text) -> m ()
    print' (path, body) = do
      withColor Blue $
        say . pack $ toFilePath path
      say body
