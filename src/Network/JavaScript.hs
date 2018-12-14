{-# LANGUAGE OverloadedStrings,KindSignatures, GADTs, ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Network.JavaScript
  ( -- * Applicative Packets of JavaScript
    Packet
  , command
  , procedure
  , constructor
    -- * sending Packets
  , send
  , sendE
    -- * Remote Value
  , RemoteValue
  , var
  , delete
    -- * Events
  , JavaScriptException(..)
  , addListener
    -- * Web services
  , start
  , Engine
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

-- | Deep embedding of an applicative packet
newtype Packet a = Packet (AF Primitive a)
  deriving (Functor, Applicative)

data Primitive :: * -> * where
  Command   :: LT.Text -> Primitive ()
  Procedure :: LT.Text -> Primitive Value
  Constructor :: LT.Text -> Primitive RemoteValue

data RemoteValue = RemoteValue Int
  deriving (Eq, Ord, Show)

var :: RemoteValue -> LT.Text
var (RemoteValue n) = "jsb.c" <> LT.pack (show n)

incRemoteValue :: RemoteValue -> RemoteValue
incRemoteValue rv = rv

-- | This accepts WebSocket requests.
--
--     * All server->client requests are of type 'Text', and are evalued.
--     * All client->server requests are of type 'Value'.
--     * Any client->server requests that are are an Object,
--       with a tag called 'jsb', are used to denode procedural replies.
--
-- listeners are added using the 'Engine' handle

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
  listenerRef <- newTVarIO $ \ _ -> return ()
  let catchMe m = try m >>= \ (_ :: Either SomeException ()) -> return ()
  _ <- forkIO $ catchMe $ forever $ do
    d <- WS.receiveData conn
--    print d
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
      Just (Event event) -> do kV <- atomically $ readTVar listenerRef
                               kV event
      Nothing -> print ("bad (non JSON) reply from JavaScript"::String,d)

  kE $ Engine
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
     , listener = listenerRef
     }

-- | An 'Engine' is a handle to a specific JavaScript engine
data Engine = Engine
  { sendText :: LT.Text -> IO ()      -- send text to the JS engine
  , genNonce ::            IO Int     -- nonce generator
  , replyBox :: Int     -> IO (Either Value [Value]) -- reply mailbox
  , listener :: TVar (Value -> IO ()) -- listener(s)
  }


bootstrap :: LT.Text
bootstrap =   LT.unlines
   [     "jsb.onmessage = function(evt){ "
   ,     "   var debug = false;"
   ,     "   var error = function(n,err) {"
   ,     "         jsb.send(JSON.stringify({id: n, error: err}));"
   ,     "         throw(err);"
   ,     "   };"
   ,     "   var event = function(ev) {"
   ,     "         jsb.send(JSON.stringify({event: ev}));"
   ,     "   };"
   ,     "   var reply = function(n,obj) {"
   ,     "       Promise.all(obj).then(function(obj){"
   ,     "         if (debug) { console.log('reply',{id:n, result:obj}); }"   
   ,     "         jsb.send(JSON.stringify({id: n, result: obj}));"
   ,     "       }).catch(function(err){"
   ,     "         error(n,err);"
   ,     "       });"
   ,     "   };"
   ,     "   if (debug) { console.log('eval',evt.data); }"
   ,     "   eval('(function(){' + evt.data + '})()');"
   ,     "};"
   ]

-- | Add a listener for events. These are chained.
--
--   From javascript, you can call event(..) to send
--   values to this listener. Any valid JSON value can be sent.
addListener :: Engine -> (Value -> IO ()) -> IO ()
addListener engine k = atomically $ modifyTVar (listener engine) $ \ f v -> f v >> k v

send :: Engine -> Packet a -> IO a
send e p = do
  r <- sendE e p
  case r of
    Right a -> return a
    Left err -> throwIO $ JavaScriptException err

sendE :: Engine -> Packet a -> IO (Either Value a)
sendE e@Engine{..} (Packet af) = prepareStmtA genNonce af >>= sendStmtA e

prepareStmtA :: Applicative f => f Int -> AF Primitive a -> f (AF Stmt a)
prepareStmtA _  (PureAF a) = pure (pure a)
prepareStmtA ug (PrimAF p) = PrimAF <$> prepareStmt ug p
prepareStmtA ug (ApAF g h) = ApAF <$> prepareStmtA ug g <*> prepareStmtA ug h

prepareStmt :: Applicative f => f Int -> Primitive a -> f (Stmt a)
prepareStmt ug (Command stmt)     = pure $ CommandStmt stmt
prepareStmt ug (Procedure stmt)   = (\ i -> ProcedureStmt i stmt) <$> ug
prepareStmt ug (Constructor stmt)   = (\ i -> ConstructorStmt (RemoteValue i) stmt) <$> ug

------------------------------------------------------------------------------

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

data JavaScriptException = JavaScriptException Value
    deriving Show

instance Exception JavaScriptException

------------------------------------------------------------------------------

class Remote f where
  -- | 'command' statement to execute in JavaScript. ';' is not needed as a terminator.
  --   Should never throw an exception, which may be reported to console.log.
  command :: LT.Text -> f ()
  -- | 'procedure' expression to execute in JavaScript. ';' is not needed as a terminator.
  --   Should never throw an exception, but any exceptions are returned to the 'send'
  --   as Haskell exceptions.
  --
  --   Procedures can return Promises. Before completing the transaction, all the values
  --   for all the procedures that are promises are fulfilled (using Promises.all).
  --
  --  If a procedure throws an exception, future commands and procedures in
  --  the same packet will not be executed. Use promises to allow all commands and
  --  procedures to be invoked, if needed.
  procedure :: LT.Text -> f Value
  -- | 'constructor' expression to execute in JavaScript. ';' is not needed as a terminator.
  --   Should never throw an exception, but any exceptions are returned to the 'send'
  --   as Haskell exceptions.
  --
  --   The value returned in not returned to Haskell. Instead, a handle is returned,
  --   that can be used to access the remote value. Examples of remote values include
  --   objects that can not be serialized, or values that are too large to serialize.
  constructor :: LT.Text -> f RemoteValue  

-- | 'delete' a remote value.
delete :: Remote f => RemoteValue -> f ()
delete rv = command $ "delete " <> var rv

instance Remote Packet where
  command = Packet . PrimAF . Command  
  procedure = Packet . PrimAF . Procedure
  constructor = Packet . PrimAF . Constructor

------------------------------------------------------------------------------
-- Framework types for Applicative and Monad

data AF :: (* -> *) -> * -> * where
 PureAF :: a -> AF m a
 PrimAF :: m a -> AF m a
 ApAF :: AF m (a -> b) -> AF m a -> AF m b

instance Functor (AF m) where
  fmap f g = pure f <*> g
  
instance Applicative (AF m) where
  pure = PureAF
  (<*>) = ApAF

concatAF :: (forall a . m a -> Maybe b) -> AF m a -> [b]
concatAF f (PureAF _) = []
concatAF f (PrimAF p) = case f p of
  Nothing -> []
  Just r -> [r]
concatAF f (ApAF m1 m2) = concatAF f m1 ++ concatAF f m2

evalAF :: Applicative f => (forall a . m a -> f a) -> AF m a -> f a
evalAF _ (PureAF a) = pure a
evalAF f (PrimAF p) = f p
evalAF f (ApAF g h) = evalAF f g <*> evalAF f h

------------------------------------------------------------------------------

-- statements are internal single JavaScript statements, that can be
-- transliterated trivially into JavaScript, or interpreted to give
-- a remote effect, including result.

data Stmt a where
  CommandStmt   :: LT.Text -> Stmt ()
  ProcedureStmt :: Int -> LT.Text -> Stmt Value
  ConstructorStmt :: RemoteValue -> LT.Text -> Stmt RemoteValue
  
showStmt :: Stmt a -> LT.Text
showStmt (CommandStmt cmd)     = cmd <> ";"
showStmt (ProcedureStmt n cmd) = "var " <> procVar n <> "=" <> cmd <> ";"
showStmt (ConstructorStmt rv cmd) = var rv <> "=" <> cmd <> ";"

evalStmt :: Stmt a -> State [Value] a
evalStmt (CommandStmt _)       = pure ()
evalStmt (ProcedureStmt _ _)   = state $ \ (v:vs) -> (v,vs)
evalStmt (ConstructorStmt c _) = pure c

-- TODO: Consider a wrapper around this Int
procVar :: Int -> LT.Text
procVar n = "v" <> LT.pack (show n)

sendStmtA :: Engine -> AF Stmt a -> IO (Either Value a)
sendStmtA e af
    | null assignments = do
        sendText e serialized
        return $ Right $ evalState (evalAF evalStmt af) []
    | otherwise = do
        nonce <- genNonce e
        sendText e $ catchMe nonce serialized
        theReply <- replyBox e nonce
        case theReply of
          Right replies -> return $ Right $ evalState (evalAF evalStmt af) replies
          Left err -> return $ Left err
        
  where
    catchMe :: Int -> LT.Text -> LT.Text
    catchMe nonce txt =
      "try{" <> txt <> "}catch(err){error(" <> LT.pack (show nonce) <> ",err);};" <>
      reply nonce <> ";"

    assignments :: [Int]
    assignments = concatAF findAssign af

    findAssign :: Stmt a -> Maybe Int
    findAssign (ProcedureStmt i _) = Just i
    findAssign _ = Nothing
    
    serialized :: LT.Text
    serialized = LT.concat $ concatAF (return . showStmt) af

    -- generate the call to reply (as a final command)
    reply :: Int -> LT.Text
    reply n =
      "reply(" <> LT.intercalate ","
      [ LT.pack (show n)
      , "[" <> LT.intercalate "," (map procVar assignments) <> "]"
      ] <> ")"

