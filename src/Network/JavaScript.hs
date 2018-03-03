{-# LANGUAGE OverloadedStrings,KindSignatures, GADTs #-}

module Network.JavaScript where
	
import Data.Monoid
import qualified Data.Text.Lazy as LT
import qualified Network.Wai.Handler.WebSockets as WS
import Network.Wai (Application)
import Network.WebSockets as WS
--import Control.Applicative.Free

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

-- 'Command' is a valid JavaScript expression.
newtype Command = Command LT.Text

command :: LT.Text -> Command
command = Command

instance Monoid Command where
  mempty                        = Command mempty		-- undefined
  Command a `mappend` Command b = Command (a `mappend` b)	-- [A,B][1]

sendCommand :: Engine -> Command -> IO ()
sendCommand (Engine conn _) (Command txt) = WS.sendTextData conn txt

{-
   var x = f(..)
   jsb.send(JSON.stringify(x));
OR   
   g(function(x) { jsb.send(JSON.stringify(x)); });

 -}

data Promise :: * -> * where
  Promise    :: (Value -> a) -> LT.Text -> Promise a
  Now        :: a            -> LT.Text -> Promise a
  Pure       :: a                       -> Promise a

instance Applicative Promise where
  pure = Pure
  -- There are 9 possible ways of building a compound promise.
--  Promise f1 t1  <*> m = 
--  Now t1         <*> m = 
  Promise k1 t1  <*> Promise k2 t2 = Promise (\ v -> k1 v (k2 v)) $ ("Promise.all([" <> t1 <> "," <> t2 <> "])")
  Promise k t1   <*> Now v t2    = Promise (flip k v) $ ("[" <> t1 <> "," <> t2 <> "][0]")
  Promise k t1   <*> Pure x      = Promise (flip k x) t1
  Now v t1       <*> Promise k t2  = Promise (v . k) ("[" <> t1 <> "," <> t2 <> "][1]")
  Now v1 t1      <*> Now v2 t2   = Now (v1 v2) ("[" <> t1 <> "," <> t2 <> "][1]") -- could return either, or undefined
  Now v t        <*> Pure x      = Now (v x) t
  Pure f         <*> Promise k t = Promise (f . k) t
  Pure f         <*> Now v t     = Now (f v) t
  Pure f         <*> Pure x      = Pure (f x)

instance Functor Promise where
  fmap f m = pure f <*> m

{-
sendPromise :: Engine -> Promise a -> IO a
sendPromise (Engine conn _) p = 
  where f (Promise a) = (
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
	

-- newtype Fragement = Fragment 

data Primitive :: * -> * where
  Command' :: LT.Text -> Primitive ()
  Procedure' :: LT.Text -> Primitive Value
  Promise'   :: LT.Text -> Primitive Value
  Allocator' :: LT.Text -> Primitive Index

--type Packet = Ap Primitive

--putText :: LT.Text -> PtkOut
--alloc
{-
packet :: Packet a -> Int -> Int -> [LT.Text]
packet (Pure _)              _ _ = []
packet (Ap (Command' t) r)   x y = t : packet r x y
packet (Ap (Procedure' t) r) x y = ("var v" <> show' x <> "=" <> t) : packet r (x+1) y
packet (Ap (Allocator' t) r) x y = ("jsb.alloc[" <> show' x <> "]=" <> t) : packet r x (y+1)
-}
show' :: Int -> LT.Text
show' n = LT.pack (show n)

newtype Index = Index Int
  
