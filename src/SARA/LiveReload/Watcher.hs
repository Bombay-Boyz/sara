{-# LANGUAGE OverloadedStrings #-}

module SARA.LiveReload.Watcher
  ( watchProject
  ) where

import System.FSNotify
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar (newEmptyMVar, tryPutMVar, tryTakeMVar, takeMVar)
import Control.Monad (forever, when, void)
import System.FilePath (takeFileName, normalise, splitDirectories)

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
    _ <- watchTree mgr root (filterEvents) $ \event -> do
      -- Filter out events inside _site or _build
      when (filterEvents event) $ do
        putStrLn $ "Change detected: " ++ show event
        void $ tryPutMVar debounceMVar ()
    
    forever $ threadDelay 1000000

filterEvents :: Event -> Bool
filterEvents event =
  let path = eventPath event
      name = takeFileName path
      -- 'filterEvents' only tests set membership below ("_site" \`elem\`
      -- parts), so 'splitDirectories'\'s top-down segment order (vs.
      -- the hand-rolled bottom-up order this used to build) doesn't
      -- change the result — this drops a locally-maintained recursive
      -- path splitter for the same function 'System.FilePath' (already
      -- imported here) already provides.
      parts = splitDirectories (normalise path)
  in not ("_site" `elem` parts) && 
     not ("_build" `elem` parts) &&
     case name of
       ('.' : _) -> False
       []        -> False
       _         -> True
