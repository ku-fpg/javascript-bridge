{-# LANGUAGE OverloadedStrings,KindSignatures, GADTs, ScopedTypeVariables #-}

module Network.JavaScript where
        
import Data.Monoid
import Data.Foldable as F
import qualified Data.Text.Lazy as LT
import qualified Network.Wai.Handler.WebSockets as WS
import Network.Wai (Application)
import qualified Network.WebSockets as WS
import Control.Monad.Trans.State

import Control.Concurrent (forkIO)
import Control.Exception (try, SomeException)
import Control.Monad (forever)
import Control.Concurrent.STM
import Data.Aeson (Value(..), decode')
import qualified Data.IntMap.Strict as IM
import Data.Scientific (toBoundedInteger)

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
-- listeners are added using the 'Engine IO' handle

start :: (Engine IO -> IO ())
      -> Application -> Application
start kE = WS.websocketsOr WS.defaultConnectionOptions $ \ pc -> do
  conn <- WS.acceptRequest pc
  nonceRef <- newTVarIO 0
  replyMap <- newTVarIO IM.empty
  listenerRef <- newTVarIO $ \ _ -> return ()
  let catchMe m = try m >>= \ (_ :: Either SomeException ()) -> return ()
  _ <- forkIO $ catchMe $ forever $ do
    d <- WS.receiveData conn
    case decode' d of
      Just (Array v) -> case F.toList v of
          (Number sci:replies) -> case toBoundedInteger sci of
              Nothing -> return ()
              Just n -> atomically
                      $ modifyTVar replyMap
                      $ IM.insert n
                      $ replies
          _ -> return ()  -- non number reply, or empty list
      Just j -> do kV <- atomically $ readTVar listenerRef
                   kV j      -- non-array
      _      -> return () -- throw away bad (non-JSON) packet

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
data Engine m = Engine
  { sendText :: LT.Text -> m ()      -- send text to the JS engine
  , genNonce ::            m Int     -- nonce generator
  , replyBox :: Int     -> m [Value] -- reply mailbox
  , listener :: TVar (Value -> IO ()) -- listener(s)
  }

addListener :: Engine IO -> (Value -> IO ()) -> IO ()
addListener engine k = atomically $ modifyTVar (listener engine) $ \ f v -> f v >> k v
              
command :: LT.Text -> Packet ()
command = Command

procedure :: LT.Text -> Packet Value
procedure = Procedure

send :: (Packetize p, Monad m) => Engine m -> p a -> m a
send engine p
   | null assignments
      = do sendText engine $ serialize stmts
           return $ evalState (patchReplies ps) []
   | otherwise = do
        nonce <- genNonce engine
        sendText engine $ serialize (reply nonce : stmts)
        replies <- replyBox engine nonce
        return $ evalState (patchReplies ps) replies
  where
    ps = packetize p

    (_,(_,stmts)) = runState (genPacket ps) (0,[])

    serialize :: [PacketStmt] ->LT.Text
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
    reply :: Int -> PacketStmt
    reply n = CommandStmt
            $ "reply(" <> LT.intercalate ","
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

class Packetize p where
  packetize :: p a -> Packet a

instance Packetize Packet where
  packetize = id
