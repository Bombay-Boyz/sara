{-# LANGUAGE OverloadedStrings #-}

module SARA.LiveReload.Server
  ( startLiveReloadServer
  , broadcastReload
  , broadcastMessage
  , broadcastToPath
  , liveReloadApp
  , ClientList
  , LiveReloadState(..)
  ) where

import Control.Concurrent (MVar, newMVar, modifyMVar_)
import Control.Concurrent.Async (forConcurrently)
import Control.Monad (forever, void)
import Control.Exception (handle, SomeException, finally)
import qualified Network.WebSockets as WS
import qualified Network.Wai.Handler.WebSockets as WaiWS
import Network.Wai (Application, responseLBS, pathInfo, remoteHost)
import Network.HTTP.Types (status200, status403)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import Network.Socket (SockAddr(..))
import Control.Concurrent.STM (TVar, newTVarIO, atomically, readTVar, writeTVar)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

data ManagedClient = ManagedClient
  { mcConn :: !WS.Connection
  , mcId   :: !Int
  }

instance Eq ManagedClient where
  a == b = mcId a == mcId b

instance Ord ManagedClient where
  compare a b = compare (mcId a) (mcId b)

type ClientList = Map.Map ManagedClient Text

data LiveReloadState = LiveReloadState
  { lrsClients :: !(MVar ClientList)
  , lrsCounter :: !(TVar Int)
  }

-- | Initialize live reload state.
startLiveReloadServer :: IO LiveReloadState
startLiveReloadServer = LiveReloadState <$> newMVar Map.empty <*> newTVarIO 0

-- | Broadcast a JSON message to all connected clients.
broadcastMessage :: Aeson.ToJSON a => LiveReloadState -> a -> IO ()
broadcastMessage state msg = modifyMVar_ (lrsClients state) $ \clientMap -> do
  let payload = Aeson.encode msg
  let clients = Map.keys clientMap
  results <- forConcurrently clients (sendMessage payload)
  let successfulClients = [c | (c, True) <- zip clients results]
  pure $ Map.filterWithKey (\client _ -> client `elem` successfulClients) clientMap
  where
    sendMessage payload client = handle (\(_ :: SomeException) -> pure False) $ do
      WS.sendDataMessage (mcConn client) (WS.Text payload Nothing)
      pure True

-- | Broadcast a JSON message only to clients viewing a specific path.
broadcastToPath :: Aeson.ToJSON a => LiveReloadState -> Text -> a -> IO ()
broadcastToPath state path msg = modifyMVar_ (lrsClients state) $ \clientMap -> do
  let payload = Aeson.encode msg
  let targetClients = Map.keys $ Map.filter (== path) clientMap
  results <- forConcurrently targetClients (sendMessage payload)
  let successfulTargetClients = [c | (c, True) <- zip targetClients results]
  -- Industrial fix: prune target clients that failed
  let prunnedMap = Map.filterWithKey (\c p -> p /= path || c `elem` successfulTargetClients) clientMap
  pure prunnedMap
  where
    sendMessage payload client = handle (\(_ :: SomeException) -> pure False) $ do
      WS.sendDataMessage (mcConn client) (WS.Text payload Nothing)
      pure True

-- | Legacy helper
broadcastReload :: LiveReloadState -> IO ()
broadcastReload state = broadcastMessage state (Aeson.object ["type" Aeson..= ("reload" :: Text)])

-- | Wai application that serves static files and handles WebSockets.

liveReloadApp :: LiveReloadState -> FilePath -> Application
liveReloadApp state siteDir req respond = do
  let path = pathInfo req
  case path of
    [".sara-internal"] -> do
      -- Fix H-06: Check if user has a file at this path before hijacking
      userFileExists <- doesFileExist (siteDir </> ".sara-internal")
      if userFileExists
        then staticApp siteDir req respond
        else if isLoopback (remoteHost req)
             then respond $ responseLBS status200 [("Content-Type", "text/html")] (BSL.fromStrict $ TE.encodeUtf8 dashboardHtml)
             else respond $ responseLBS status403 [("Content-Type", "text/plain")] "Access denied: Localhost only"
    _ -> WaiWS.websocketsOr WS.defaultConnectionOptions 
          (\pending -> if WS.requestPath (WS.pendingRequest pending) == "/live"
                       then wsApp state pending
                       else WS.rejectRequest pending "Not a live reload path") 
          (staticApp siteDir) req respond

isLoopback :: SockAddr -> Bool
isLoopback (SockAddrInet _ host) = host == 0x0100007f -- 127.0.0.1
isLoopback (SockAddrInet6 _ _ (0,0,0,1) _) = True    -- ::1
isLoopback _ = False

staticApp :: FilePath -> Application
staticApp siteDir = staticPolicy (addBase siteDir) $ \_ respond ->
  respond $ responseLBS status200 [("Content-Type", "text/plain")] "SARA Development Server"

wsApp :: LiveReloadState -> WS.ServerApp
wsApp state pending = do
  conn <- WS.acceptRequest pending
  msg <- WS.receiveData conn :: IO Text
  case Aeson.decode (BSL.fromStrict $ TE.encodeUtf8 msg) of
    Just (Aeson.Object obj) | Just (Aeson.String path) <- Map.lookup "path" (toDataKeyMap obj) -> do
       newId <- atomically $ do
         curr <- readTVar (lrsCounter state)
         let next = curr + 1
         writeTVar (lrsCounter state) next
         return next
       let client = ManagedClient conn newId
       modifyMVar_ (lrsClients state) $ \clientMap -> pure (Map.insert client path clientMap)
       putStrLn $ "Client subscribed to " ++ T.unpack path
       -- Industrial fix: ensure client is removed on disconnect
       handle (\(_ :: SomeException) -> return ()) $ 
         finally (forever $ void (WS.receiveData conn :: IO Text)) $
           modifyMVar_ (lrsClients state) $ \clientMap -> pure (Map.delete client clientMap)
    _ -> WS.sendClose conn ("Expected subscribe message" :: Text)

toDataKeyMap :: KM.KeyMap Aeson.Value -> Map.Map Text Aeson.Value
toDataKeyMap = Map.fromList . map (\(k, v) -> (K.toText k, v)) . KM.toList

dashboardHtml :: Text
dashboardHtml = T.unlines
  [ "<!DOCTYPE html>"
  , "<html>"
  , "<head>"
  , "  <title>SARA Industrial Dashboard</title>"
  , "  <style>"
  , "    body { font-family: system-ui; background: #0f0f0f; color: #eee; margin: 0; display: flex; align-items: center; justify-content: center; height: 100vh; }"
  , "    .dash { background: rgba(255,255,255,0.05); backdrop-filter: blur(10px); border: 1px solid rgba(255,255,255,0.1); padding: 40px; border-radius: 20px; width: 400px; box-shadow: 0 20px 50px rgba(0,0,0,0.5); }"
  , "    h1 { margin: 0 0 20px; font-size: 1.5rem; text-align: center; color: #0070f3; text-transform: uppercase; letter-spacing: 2px; }"
  , "    .stat { display: flex; justify-content: space-between; margin-bottom: 15px; font-size: 1.1rem; }"
  , "    .label { color: #888; }"
  , "    .val { font-weight: bold; }"
  , "    .status-ok { color: #00ff00; }"
  , "    .status-warn { color: #ffff00; }"
  , "    .status-fail { color: #ff0000; }"
  , "  </style>"
  , "</head>"
  , "<body>"
  , "  <div class=\"dash\">"
  , "    <h1>SARA Health</h1>"
  , "    <div id=\"stats\">"
  , "      <div class=\"stat\"><span class=\"label\">Security</span><span class=\"val\" id=\"security\">-</span></div>"
  , "      <div class=\"stat\"><span class=\"label\">SEO</span><span class=\"val\" id=\"seo\">-</span></div>"
  , "      <div class=\"stat\"><span class=\"label\">Performance</span><span class=\"val\" id=\"perf\">-</span></div>"
  , "      <div class=\"stat\"><span class=\"label\">Pages</span><span class=\"val\" id=\"pages\">-</span></div>"
  , "    </div>"
  , "  </div>"
  , "  <script>"
  , "    function connect() {"
  , "      const socket = new WebSocket('ws://' + location.host + '/live');"
  , "      socket.onopen = () => {"
  , "        console.log('SARA: Connected to health monitor');"
  , "        socket.send(JSON.stringify({path: '/.sara-internal'}));"
  , "      };"
  , "      socket.onmessage = (event) => {"
  , "        const msg = JSON.parse(event.data);"
  , "        if (msg.type === 'quality-seal') {"
  , "          const data = msg.data;"
  , "          document.getElementById('security').innerText = data.qsSecurity ? 'IRONCLAD' : 'VULNERABLE';"
  , "          document.getElementById('security').className = data.qsSecurity ? 'val status-ok' : 'val status-fail';"
  , "          document.getElementById('seo').innerText = data.qsSEO ? 'PASS' : 'ISSUES';"
  , "          document.getElementById('seo').className = data.qsSEO ? 'val status-ok' : 'val status-warn';"
  , "          document.getElementById('perf').innerText = data.qsPerformance + '/100';"
  , "          document.getElementById('pages').innerText = data.qsItemCount;"
  , "        }"
  , "      };"
  , "      socket.onclose = () => {"
  , "        console.log('SARA: Connection lost. Reconnecting in 2s...');"
  , "        setTimeout(connect, 2000);"
  , "      };"
  , "    }"
  , "    connect();"
  , "  </script>"
  , "</body>"
  , "</html>"
  ]
