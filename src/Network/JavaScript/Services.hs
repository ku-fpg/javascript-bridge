{-# LANGUAGE OverloadedStrings,KindSignatures, GADTs, ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Network.JavaScript.Services
  ( -- * Web Services
    Engine(..)
  , start
  , addListener
  , Application
  ) where

import Control.Applicative((<|>),liftA2)
import Control.Exception(Exception)
import Control.Exception as Exception
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as LT
import qualified Network.Wai.Handler.WebSockets as WS
import Network.Wai (Application)
import qualified Network.WebSockets as WS
import Control.Monad.Trans.State.Strict

import Control.Concurrent (forkIO)
import Control.Exception (try, SomeException)
import Control.Monad (forever)
import Control.Concurrent.STM
import Data.Aeson (Value(..), decode', FromJSON(..),withObject,(.:))
import qualified Data.IntMap.Strict as IM

import Network.JavaScript.Reactive(Event, eventIO, forkE, ThreadId)

-- | This accepts WebSocket requests.
--
--     * All server->client requests are of type 'Text', and are evalued.
--     * All client->server requests are of type 'Value'.
--     * Any client->server requests that are are an Object,
--       with a tag called 'jsb', are used to denode procedural replies.
--
-- listeners are added using the 'Engine' handle

start :: (Event Value -> Engine -> IO ())
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
  eventQueue <- newTQueueIO
  events <- eventIO $ atomically $ readTQueue eventQueue
  
  let catchMe m = try m >>= \ (_ :: Either SomeException ()) -> return ()
  _ <- forkIO $ catchMe $ forever $ do
    d <- WS.receiveData conn
    print d
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
      Just (Event event) -> atomically $ writeTQueue eventQueue event
      Nothing -> print ("bad (non JSON) reply from JavaScript"::String,d)

  kE events $ Engine
     { sendText = WS.sendTextData conn
     , genNonce = atomically $ do
         n <- readTVar nonceRef
         writeTVar nonceRef $ succ n
         return n
     , replyBox = \ n -> atomically $ do
         t <- readTVar replyMap
         case IM.lookup n t of
           Nothing -> retry
           Just v -> return v
     }

-- | An 'Engine' is a handle to a specific JavaScript engine
data Engine = Engine
  { sendText :: LT.Text -> IO ()      -- send text to the JS engine
  , genNonce ::            IO Int     -- nonce generator
  , replyBox :: Int     -> IO (Either Value [Value]) -- reply mailbox
  }

bootstrap :: LT.Text
bootstrap =   LT.unlines
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

-- | Add a listener for events. There can be many.
--
--   From javascript, you can call event(..) to send
--   values to this listener. Any valid JSON value can be sent.
addListener :: Event Value -> (Value -> IO ()) -> IO ThreadId
addListener events k = forkE k events

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

