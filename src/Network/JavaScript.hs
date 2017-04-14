{-# LANGUAGE OverloadedStrings,KindSignatures, GADTs #-}

module Network.JavaScript where
	
import Data.Monoid
import qualified Data.Text.Lazy as LT
import qualified Network.Wai.Handler.WebSockets as WS
import Network.Wai (Application)
import Network.WebSockets as WS

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Concurrent.STM
import Data.Aeson (Value(..), decode')
import qualified Data.HashMap.Strict as HM
import Data.Scientific (toBoundedInteger)

-- | This accepts WebSocket requests. 
-- 
--     * All server->client requests are of type 'Text', and are evalued.
--     * All client->server requests are of type 'Value'.
--     * Any client->server requests that are are an Object,
--       with a tag called 'jsb', are used to denode procedural replies.

start :: (Value -> IO ()) -> (Engine -> IO ()) -> Application -> Application
start kV kE = WS.websocketsOr WS.defaultConnectionOptions $ \ pc -> do
  conn <- WS.acceptRequest pc
  replyMap <- newTVarIO HM.empty
  forkIO $ forever $ do
    d <- receiveData conn
    case decode' d of
      Just j@(Object e) ->
      	case HM.lookup "jsb" e of
         Just (Number sci) -> 
	   case toBoundedInteger sci of
	     Just n -> atomically $ modifyTVar replyMap $ HM.insert n $ j
	     Nothing -> return () -- ignore; this was a bad nonce / number.
	 Just _ -> return () -- we discard all objects with a jsb tag.
	 _   -> kV j      -- Objects
      Just j -> kV j	  -- non-object, like array or numbers.
      _      -> return () -- throw away bad (non-JSON) packet
  kE $ Engine conn replyMap

-- 'Command' is a valid JavaScript command, typically terminated by a semi-colon.
-- They can safely be appended (using the 'Monoid' operators)
newtype Command = Command LT.Text

command :: LT.Text -> Command
command = Command

instance Monoid Command where
  mempty                        = Command mempty
  Command a `mappend` Command b = Command (a `mappend` b)

sendCommand :: Engine -> Command -> IO ()
sendCommand (Engine conn _) (Command txt) = WS.sendTextData conn txt

{-
   var x = f(..)
   jsb.send(JSON.stringify(x));
OR   
   g(function(x) { jsb.send(JSON.stringify(x)); });

 -}

data Procedure :: * -> * where
  Procedure :: LT.Text                           -> Procedure Value --- expression to return.
  Context   :: LT.Text -> Procedure a -> LT.Text -> Procedure a     --- wrapper, typically involing a callback

procedure :: LT.Text -> Procedure Value
procedure = Procedure

context :: LT.Text -> Procedure a -> LT.Text -> Procedure a
context = Context

sendProcedure :: Engine -> Procedure a -> IO a
sendProcedure (Engine conn _) proc = do return undefined
{-
	let scribe (Procedure p) = "merge
	    txts = scribe proc
	WS.sendTextData conn txt
-}
----------------------------------------

data Engine = Engine 
	WS.Connection
	(TVar (HM.HashMap Int Value))

{-
-- Returns true 
newtype Callback = Callback (forall a . FromJSON a => a -> IO Bool))

-}

hack :: Engine -> IO ()
hack (Engine c m) = do
	d <- receiveData c
	print (d :: LT.Text)
	
	