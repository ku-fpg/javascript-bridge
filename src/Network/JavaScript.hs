{-# LANGUAGE OverloadedStrings,KindSignatures, GADTs, ScopedTypeVariables #-}

module Network.JavaScript
  ( -- * Applicative Packets of JavaScript
    Packet
  , command
  , procedure
    -- * sending Packets
  , send
  , sendE
    -- * Events
  , JavaScriptException(..)
  , addListener
    -- * Web services
  , start
  , Engine
  ) where
        
import Control.Applicative((<|>))
import Control.Exception(Exception)
import Control.Exception as Exception
import Data.Monoid
import qualified Data.Text.Lazy as LT
import qualified Network.Wai.Handler.WebSockets as WS
import Network.Wai (Application)
import qualified Network.WebSockets as WS
import Control.Monad.Trans.State

import Control.Concurrent (forkIO)
import Control.Exception (try, SomeException)
import Control.Monad (forever)
import Control.Concurrent.STM
import Data.Aeson (Value(..), decode', FromJSON(..),withObject,(.:))
import Data.Aeson.Types (Parser)
import qualified Data.IntMap.Strict as IM

-- | Deep embedding of an applicative packet
data Packet :: * -> * where
  Pure      :: a       -> Packet a
  Ap        ::            Packet (a -> b)
                       -> Packet a
                       -> Packet b
  Command   :: LT.Text -> Packet ()
  Procedure :: LT.Text -> Packet Value

instance Functor Packet where
  fmap f m = Pure f `Ap` m

instance Applicative Packet where
  pure = Pure
  (<*>) = Ap

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
   ,     "   var error = function(n,err) {"
   ,     "         jsb.send(JSON.stringify({haskell: true, id: n, error: err}));"
   ,     "         throw(err);"
   ,     "   };"
   ,     "   var event = function(ev) {"
   ,     "         jsb.send(JSON.stringify({event: ev}));"
   ,     "   };"
   ,     "   var reply = function(n,obj) {"
   ,     "       Promise.all(obj).then(function(obj){"
   ,     "         jsb.send(JSON.stringify({haskell: true, id: n, result: obj}));"
   ,     "       }).catch(function(err){"
   ,     "         error(n,err);"
   ,     "       });"
   ,     "   };"
--   ,     "   if (true || debug) { console.log('eval',evt.data); }"
   ,     "   eval('(function(){' + evt.data + '})()');"
   ,     "};"
   ]

-- | Add a listener for events. These are chained.
--
--   From javascript, you can call event(..) to send
--   values to this listener. Any valid JSON value can be sent.

addListener :: Engine -> (Value -> IO ()) -> IO ()
addListener engine k = atomically $ modifyTVar (listener engine) $ \ f v -> f v >> k v

-- | 'command' statement to execute in JavaScript. ';' is not needed as a terminator.
--   Should never throw an exception, which is reported to console.log.

command :: LT.Text -> Packet ()
command = Command

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

procedure :: LT.Text -> Packet Value
procedure = Procedure

send :: Engine -> Packet a -> IO a
send e p = do
  r <- sendE e p
  case r of
    Right a -> return a
    Left err -> throwIO $ JavaScriptException err

sendE :: Engine -> Packet a -> IO (Either Value a)
sendE engine ps
   | null assignments
      = do sendText engine $ serialize stmts
           return $ Right $ evalState (patchReplies ps) []
   | otherwise = do
        nonce <- genNonce engine
        sendText engine $ catchMe nonce $ serialize stmts
        theReply <- replyBox engine nonce
        case theReply of
          Right replies -> return $ Right $ evalState (patchReplies ps) replies
          Left err -> return $ Left err
  where

    (_,(_,stmts)) = runState (genPacket ps) (0,[])

    catchMe :: Int -> LT.Text -> LT.Text
    catchMe nonce txt = "try{" <> txt <> "}catch(err){error(" <> LT.pack (show nonce) <> ",err);};" <> reply nonce <> ";"

    serialize :: [PacketStmt] -> LT.Text
    serialize = LT.concat . map showStmt . reverse

    assignments :: [LT.Text]
    assignments = [ v
                  | a <- reverse stmts
                  , v <- case a of
                      CommandStmt{}     -> []
                      ProcedureStmt i _ -> [procVar i]
                  ]

    -- generate the packet to be sent
    genPacket :: Packet a -> State (Int,[PacketStmt]) ()
    genPacket Pure{}           = return ()
    genPacket (Ap g h)         = genPacket g *> genPacket h
    genPacket (Command stmt)   =
        modify $ \ (n,ss) -> (n,CommandStmt stmt : ss)
    genPacket (Procedure stmt) = 
        modify $ \ (n,ss) -> (n+1,ProcedureStmt n stmt : ss)

    -- generate the call to reply (as a final command)
    reply :: Int -> LT.Text
    reply n = "reply(" <> LT.intercalate ","
                           [ LT.pack (show n)
                           , "[" <> LT.intercalate "," assignments <> "]"
                           ] <> ")"

    patchReplies :: Packet a -> State [Value] a
    patchReplies (Pure a)    = return a
    patchReplies (Ap g h)    = patchReplies g <*> patchReplies h
    patchReplies Command{}   = return ()
    patchReplies Procedure{} = popState

    popState :: State [Value] Value
    popState = state $ \ vs -> case vs of
                 [] -> error "run out of result arguments"
                 (v:vs') -> (v,vs')
                     
data PacketStmt
   = CommandStmt       LT.Text
   | ProcedureStmt Int LT.Text
 deriving Show

showStmt :: PacketStmt -> LT.Text
showStmt (CommandStmt cmd)     = cmd <> ";"
showStmt (ProcedureStmt n cmd) = "var " <> procVar n <> "=" <> cmd <> ";"

procVar :: Int -> LT.Text
procVar n = "v" <> LT.pack (show n)

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
        <* (v .: "haskell" :: Parser Value)
      parseError = withObject "Error" $ \v -> Error
        <$> v .: "id"
        <*> v .: "error"
        <* (v .: "haskell" :: Parser Value)

data JavaScriptException = JavaScriptException Value
    deriving Show

instance Exception JavaScriptException
