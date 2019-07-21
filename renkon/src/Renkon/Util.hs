module Renkon.Util
  ( module Renkon.Util
  , module X
  ) where

import ClassyPrelude hiding ((</>))

import Path
import Renkon.Generator
import System.Console.ANSI
import System.Console.ANSI.Types as X
import System.Process


-- * System utilities

-- | Execut a command.
execute :: FilePath -> [Text] -> IO ()
execute exe args = callProcess exe $ unpack <$> args

-- | Execut a command and returns stdio.
execute' :: FilePath -> [Text] -> IO Text
execute' exe args = do
  (_, Just hout, _, _) <- createProcess (proc exe args') { std_out = CreatePipe }
  decodeUtf8 <$> hGetContents hout
  where
    args' = unpack <$> args


-- * Display functions

displayItemSimple :: Generator -> IO ()
displayItemSimple Generator {..} =
  withColor Green $ say name

displayItemDetail :: Generator -> IO ()
displayItemDetail gen'@Generator {..} = do
  displayItemSimple gen'
  withColor Yellow $
    say $ "  " <> pack (toFilePath path)
  withColor White $
    traverse_ (say . ("  " <>)) . lines =<< execute' (toFilePath path) ["--help"]
  say ""


-- * Coloring

-- | Run IO action with colored output.
withColor :: (MonadUnliftIO m) => Color -> m () -> m ()
withColor color = withSGR [SetColor Foreground Dull color]

-- | Run IO action with high-intencity colored output.
withVivid :: (MonadUnliftIO m) => Color -> m () -> m ()
withVivid color = withSGR [SetColor Foreground Vivid color]

-- | Run IO action with bold colored output.
withBold :: (MonadUnliftIO m) => Color -> m () -> m ()
withBold color = withSGR
  [ SetColor Foreground Vivid color
  , SetConsoleIntensity BoldIntensity
  ]

-- | Run IO action with given SGR context.
withSGR :: (MonadUnliftIO m) => [SGR] -> m () -> m ()
withSGR sgrs =
  bracket_
  (liftIO $ setSGR sgrs)
  (liftIO $ setSGR [Reset])
