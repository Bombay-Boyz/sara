{-# LANGUAGE OverloadedStrings #-}

module SARA.LiveReload.Watcher
  ( watchProject
  ) where

import System.FSNotify
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar (newEmptyMVar, tryPutMVar, tryTakeMVar, takeMVar)
import Control.Monad (forever, when, void)
import System.FilePath (takeFileName, splitFileName, normalise)

-- | Watches the project for changes and triggers build + callback.
--   Includes a 200ms debounce to prevent redundant builds.
watchProject :: FilePath -> IO () -> IO ()
watchProject root onEvent = do
  debounceMVar <- newEmptyMVar
  
  -- Debouncer thread
  void $ forkIO $ forever $ do
    _ <- takeMVar debounceMVar
    threadDelay 200000 -- 200ms
    -- Clear any additional notifications that came in during the delay
    void $ tryTakeMVar debounceMVar
    onEvent

  withManager $ \mgr -> do
    putStrLn $ "Watching " ++ root ++ " for changes..."
    -- Watch recursively
    _ <- watchTree mgr root filterEvents $ \event -> do
      putStrLn $ "Change detected: " ++ show event
      void $ tryPutMVar debounceMVar ()
    
    forever $ threadDelay 1000000

filterEvents :: Event -> Bool
filterEvents event =
  let path = eventPath event
      name = takeFileName path
      parts = splitPath path
  in not ("_site" `elem` parts) && 
     not ("_build" `elem` parts) &&
     not (null name) && head name /= '.'

splitPath :: FilePath -> [FilePath]
splitPath = go . normalise
  where
    go "." = []
    go "/" = []
    go ""  = []
    go p = let (d, f) = splitFileName p in f : go d
