{-# LANGUAGE OverloadedStrings,KindSignatures, GADTs, ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Network.JavaScript.Services
  ( -- * Web Services
    Engine(..)
  , start
  , addListener
  , listen
  , readEventChan
  , Application
  ) where

import Control.Applicative((<|>))
import qualified Data.Text.Lazy as LT
import Data.Time.Clock
import qualified Network.Wai.Handler.WebSockets as WS
import Network.Wai (Application)
import qualified Network.WebSockets as WS

import Control.Concurrent (forkIO, ThreadId)
import Control.Exception (try, SomeException)
import Control.Monad (forever)
import Control.Concurrent.STM
import Data.Aeson (Value(..), decode', FromJSON(..),withObject,(.:))
import qualified Data.IntMap.Strict as IM

import Network.JavaScript.Internal(JavaScript(..))

-- | This accepts WebSocket requests, calls the callback with
--   an 'Engine' that can be used to access JavaScript.

start :: (Engine -> IO ())
      -> Application -> Application
start kE = WS.websocketsOr WS.defaultConnectionOptions $ \ pc -> do
  conn <- WS.acceptRequest pc
  -- Use ping to keep connection alive
  WS.forkPingThread conn 10
  -- Bootstrap the remote handler
  WS.sendTextData conn bootstrap
  -- Handling packets
  nonceRef <- newTVarIO 0
  replyMap <- newTVarIO IM.empty
  eventQueue <- newTChanIO
  
  let catchMe m = try m >>= \ (_ :: Either SomeException ()) -> return ()
  _ <- forkIO $ catchMe $ forever $ do
    d <- WS.receiveData conn
    case decode' d of
      Just (Result _ []) -> return ()
      Just (Result n replies) -> atomically
                      $ modifyTVar replyMap
                      $ IM.insert n
                      $ Right
                      $ replies
      Just (Error n obj) -> atomically
                      $ modifyTVar replyMap
                      $ IM.insert n
                      $ Left
                      $ obj
      Just (Event event) -> do
        utc <- getCurrentTime
        atomically $ writeTChan eventQueue (event,utc)
        
      Nothing -> print ("bad (non JSON) reply from JavaScript"::String,d)

  kE $ Engine
     { sendJavaScript = \ (JavaScript js) -> WS.sendTextData conn js
     , genNonce = atomically $ do
         n <- readTVar nonceRef
         writeTVar nonceRef $ succ n
         return n
     , replyBox = \ n -> atomically $ do
         t <- readTVar replyMap
         case IM.lookup n t of
           Nothing -> retry
           Just v -> return v
     , eventChan = readTChan eventQueue
     }

-- | An 'Engine' is a handle to a specific JavaScript engine
data Engine = Engine
  { sendJavaScript :: JavaScript -> IO ()      -- send text to the JS engine
  , genNonce       ::               IO Int     -- nonce generator
  , replyBox       :: Int        -> IO (Either Value [Value]) -- reply mailbox
  , eventChan      ::               STM (Value, UTCTime)
  }

bootstrap :: LT.Text
bootstrap = LT.unlines
   [     "jsb.event =  function(ev) {"
   ,     "         if (jsb.debug) { console.log('event',{event: ev}); }"
   ,     "         jsb.ws.send(JSON.stringify({event: ev}));"
   ,     "   };"
   ,     "jsb.error = function(n,err) {"
   ,     "         if (jsb.debug) { console.log('send',{id: n, error: err}); }"
   ,     "         jsb.ws.send(JSON.stringify({id: n, error: err}));"
   ,     "         throw(err);"
   ,     "   };"   
   ,     "jsb.reply = function(n,obj) {"
   ,     "       Promise.all(obj).then(function(obj){"
   ,     "         if (jsb.debug) { console.log('reply',{id:n, result:obj}); }"   
   ,     "         jsb.ws.send(JSON.stringify({id: n, result: obj}));"
   ,     "       }).catch(function(err){"
   ,     "         jsb.error(n,err);"
   ,     "       });"
   ,     "   };"
   ,     "jsb.ws.onmessage = function(evt){ "
   ,     "   if (jsb.debug) { console.log('eval',evt.data); }"
   ,     "   eval('(function(){' + evt.data + '})()');"
   ,     "};"
   ,     "jsb.rs = [];"
   ]

--
-- | Add a listener for events. There can be many. non-blocking.
--
--   From JavaScript, you can call event(..) to send
--   values to this listener. Any valid JSON value can be sent.
addListener :: Engine -> (Value -> IO ()) -> IO ThreadId
addListener e k = forkIO $ forever $ listen e >>= k

-- | 'listen' for the next event. blocking.
--
--   From JavaScript, you can call event(..) to send
--   values to this listener. Any valid JSON value can be sent.
--
--
listen :: Engine -> IO Value
listen e = atomically $ fst <$> readEventChan e

-- | 'readEventChan' uses STM to read the next event.
--
--   From JavaScript, you can call event(..) to send
--   values to this channel. Any valid JSON value can be sent.
--
--
readEventChan :: Engine -> STM (Value, UTCTime)
readEventChan = eventChan

------------------------------------------------------------------------------

-- This is what we send back from JavaScript.
data Reply = Result Int [Value]
           | Error Int Value
           | Event Value
  deriving Show

instance FromJSON Reply where
  parseJSON o =  parseEvent o
             <|> parseResult o
             <|> parseError o
    where
      parseEvent = withObject "Event" $ \v -> Event
        <$> v .: "event"
      parseResult = withObject "Result" $ \v -> Result
        <$> v .: "id"
        <*> v .: "result"
      parseError = withObject "Error" $ \v -> Error
        <$> v .: "id"
        <*> v .: "error"

