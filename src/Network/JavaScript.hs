{-# LANGUAGE OverloadedStrings,KindSignatures, GADTs, ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -w #-}
module Network.JavaScript
  ( -- * Remote Applicative Packets of JavaScript
    Remote
  , Packet
  , RemoteMonad
  , RemoteProcedure
  , command
  , procedure
  , constructor
  , function
    -- * sending Packets
  , send
  , sendA
  , sendE
    -- * Remote and Local Values
  , RemoteValue
  , var
  , val
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
import Data.Aeson ( Value(..), decode', FromJSON(..),withObject,(.:)
                  , ToJSON(..), encode)
import Data.Text.Lazy.Encoding(decodeUtf8,encodeUtf8)
import qualified Data.Aeson.Encoding.Internal as AI
import qualified Data.Binary.Builder as B
import qualified Data.IntMap.Strict as IM


import Network.JavaScript.Services

------------------------------------------------------------------------------

class Remote f where
  -- | 'command' statement to execute in JavaScript. ';' is not needed as a terminator.
  --   Should never throw an exception, which may be reported to console.log.
  command :: LT.Text -> f ()
  -- | 'constructor' expression to execute in JavaScript. ';' is not needed as a terminator.
  --   Should never throw an exception, but any exceptions are returned to the 'send'
  --   as Haskell exceptions.
  --
  --   The value returned in not returned to Haskell. Instead, a handle is returned,
  --   that can be used to access the remote value. Examples of remote values include
  --   objects that can not be serialized, or values that are too large to serialize.
  constructor :: LT.Text -> f RemoteValue  

  -- | a 'function' takes a Haskell function, and converts
  --   it into a JavaScript function. This can be used to 
  --   generate first-class functions, for passing as arguments.
  function :: ToJSON r
           => (forall g . (Applicative g, Remote g) => RemoteValue -> g r)
           -> f RemoteValue

class Remote f => RemoteProcedure f where
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
  
-- | A sync will always flush the send queue.
sync :: (Monad f, RemoteProcedure f) => f ()
sync = procedure "null" >>= \ Null -> return ()

-- | Deep embedding of an applicative packet
newtype Packet a = Packet (AF Primitive a)
  deriving (Functor, Applicative)

-- | Deep embedding of an applicative packet
newtype RemoteMonad a = RemoteMonad (M Primitive a)
  deriving (Functor, Applicative, Monad)

data Primitive :: * -> * where
  Command   :: LT.Text -> Primitive ()
  Procedure :: LT.Text -> Primitive Value
  Constructor :: LT.Text -> Primitive RemoteValue
  Function :: ToJSON r
           => (forall g . (Applicative g, Remote g) => RemoteValue -> g r)
           -> Primitive RemoteValue

instance Remote Packet where
  command = Packet . PrimAF . Command  
  constructor = Packet . PrimAF . Constructor
  function k = Packet $ PrimAF $ Function k

instance RemoteProcedure Packet where
  procedure = Packet . PrimAF . Procedure

instance Remote RemoteMonad where
  command = RemoteMonad . PrimM . Command  
  constructor = RemoteMonad . PrimM . Constructor
  function k = RemoteMonad $ PrimM $ Function k

instance RemoteProcedure RemoteMonad where
  procedure = RemoteMonad . PrimM . Procedure  

------------------------------------------------------------------------------

send :: Engine -> RemoteMonad a -> IO a
send e p = do
  r <- sendE e p
  case r of
    Right a -> return a
    Left err -> throwIO $ JavaScriptException err

data JavaScriptException = JavaScriptException Value
    deriving Show

instance Exception JavaScriptException

sendE :: Engine -> RemoteMonad a -> IO (Either Value a)
sendE e (RemoteMonad m) = go m
  where
    go m = do
      w <- walkStmtM e m
      case w of
        ResultPacket af _ -> sendStmtA e af
        IntermPacket af k -> do
          r <- sendStmtA e af
          case r of
            Right a -> go (k a)
            Left msg -> return $ Left msg

data PingPong a where
  ResultPacket :: AF Stmt a -> Maybe a -> PingPong a
  IntermPacket :: AF Stmt a -> (a -> M Primitive b) -> PingPong b

walkStmtM :: Engine -> M Primitive a -> IO (PingPong a)
walkStmtM _          (PureM a) = pure $ ResultPacket (pure a) (pure a)
walkStmtM Engine{..} (PrimM p) = do
  s <- prepareStmt genNonce p
  let af = PrimAF s
  return $ ResultPacket af (evalStmtA af [])
walkStmtM e          (ApM g h) = do
  w1 <- walkStmtM e g
  case w1 of
    ResultPacket g_af g_r -> do
      w2 <- walkStmtM e h
      case w2 of
        ResultPacket h_af h_r -> return $ ResultPacket (g_af <*> h_af) (liftA2 ($) g_r h_r)
        IntermPacket h_af k -> return $
          IntermPacket (liftA2 (,) g_af h_af)
                       (\ (r1,r2) -> pure r1 <*> k r2)
    IntermPacket g_af k -> return $ IntermPacket g_af (\ r -> k r <*> h)
walkStmtM e          (BindM m k) = do
  w1 <- walkStmtM e m
  case w1 of
    ResultPacket m_af (Just a) -> do
      w2 <- walkStmtM e (k a)
      case w2 of
        ResultPacket h_af h_r ->
          return $ ResultPacket (m_af *> h_af) h_r
        IntermPacket h_af k -> return $
          IntermPacket (m_af *> h_af) k
    ResultPacket m_af Nothing ->
          return $ IntermPacket m_af k
    IntermPacket m_af k0 -> return $ IntermPacket m_af (\ r -> k0 r >>= k)

sendA :: Engine -> Packet a -> IO a
sendA e p = do
  r <- sendAE e p
  case r of
    Right a -> return a
    Left err -> throwIO $ JavaScriptException err

-- INLINE
sendAE :: Engine -> Packet a -> IO (Either Value a)
sendAE e@Engine{..} (Packet af) = prepareStmtA genNonce af >>= sendStmtA e

-- statements are internal single JavaScript statements, that can be
-- transliterated trivially into JavaScript, or interpreted to give
-- a remote effect, including result.

data Stmt a where
  CommandStmt   :: LT.Text -> Stmt ()
  ProcedureStmt :: Int -> LT.Text -> Stmt Value
  ConstructorStmt :: RemoteValue -> LT.Text -> Stmt RemoteValue

deriving instance Show (Stmt a)
  
prepareStmtA :: Monad f => f Int -> AF Primitive a -> f (AF Stmt a)
prepareStmtA _  (PureAF a) = pure (pure a)
prepareStmtA ug (PrimAF p) = PrimAF <$> prepareStmt ug p
prepareStmtA ug (ApAF g h) = ApAF <$> prepareStmtA ug g <*> prepareStmtA ug h

prepareStmt :: Monad f => f Int -> Primitive a -> f (Stmt a)
prepareStmt ug (Command stmt)     = pure $ CommandStmt stmt
prepareStmt ug (Procedure stmt)   = ug >>= \ i -> pure $ ProcedureStmt i stmt
prepareStmt ug (Constructor stmt) = ug >>= \ i -> pure $ ConstructorStmt (RemoteValue i) stmt
prepareStmt ug (Function k) = do
  i <- ug
  let a0 = RemoteArgument i
  let Packet f = k a0
  ss <- prepareStmtA ug $ f 
  case evalStmtA ss [] of
    Nothing -> error "procedure inside function (should never happen)"
    Just a -> do
      j <- ug
      -- Technically, we can handle a single procedure,
      -- as the final primitive, but using return (..the prim..).
      let funWrapper xs = "function(" <> val a0 <> "){" <> xs <> "return " <> val a <> ";}"
      return $ ConstructorStmt (RemoteValue j)
             $ funWrapper
             $ serializeA ss
   
showStmtA :: AF Stmt a -> LT.Text
showStmtA = LT.concat . concatAF (return . showStmt)

showStmt :: Stmt a -> LT.Text
showStmt (CommandStmt cmd)     = cmd <> ";"
showStmt (ProcedureStmt n cmd) = "var " <> procVar n <> "=" <> cmd <> ";"
showStmt (ConstructorStmt rv cmd) = var rv <> "=" <> cmd <> ";"

evalStmtA :: AF Stmt a -> [Value] -> Maybe a
evalStmtA af st = evalStateT (evalAF evalStmt af) st

evalStmt :: Stmt a -> StateT [Value] Maybe a
evalStmt (CommandStmt _)       = pure ()
evalStmt (ProcedureStmt _ _)   = do
  vs <- get
  case vs of
    (v:vs') -> put vs' >> return v
    _ -> fail "not enough values"
evalStmt (ConstructorStmt c _) = pure c

serializeA :: AF Stmt a -> LT.Text
serializeA = LT.concat . concatAF (return . showStmt)

sendStmtA :: Engine -> AF Stmt a -> IO (Either Value a)
sendStmtA e (PureAF a) = return (pure a)
sendStmtA e af
    | null assignments = do
        sendText e $ serializeA af
        return $ case evalStmtA af [] of
            Nothing -> error "internal failure"
            Just r -> Right r        
    | otherwise = do
        nonce <- genNonce e
        sendText e $ catchMe nonce $ serializeA af
        theReply <- replyBox e nonce
        case theReply of
          Right replies -> return $ case evalStmtA af replies of
            Nothing -> error "internal failure"
            Just r -> Right r
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
    
    -- generate the call to reply (as a final command)
    reply :: Int -> LT.Text
    reply n =
      "reply(" <> LT.intercalate ","
      [ LT.pack (show n)
      , "[" <> LT.intercalate "," (map procVar assignments) <> "]"
      ] <> ")"

-- TODO: Consider a wrapper around this Int
procVar :: Int -> LT.Text
procVar n = "v" <> LT.pack (show n)

------------------------------------------------------------------------------

-- A Local handle into a remote value.
data RemoteValue = RemoteValue Int
                 | RemoteArgument Int
  deriving (Eq, Ord, Show)

-- Remote values can not be encoded in JSON, but are JavaScript variables.
instance ToJSON RemoteValue where
  toJSON = error "toJSON not supported for RemoteValue"
  toEncoding = AI.unsafeToEncoding . B.fromLazyByteString . encodeUtf8 . var

-- | generate the text for a JavaScript value, including RemoteValues.
val :: ToJSON v => v -> LT.Text
val = decodeUtf8 . encode

-- | generate the text for a RemoteValue
var :: RemoteValue -> LT.Text
var (RemoteValue n) = "jsb.c" <> LT.pack (show n)
var (RemoteArgument n) = "a" <> LT.pack (show n)

-- | 'delete' a remote value.
delete :: Remote f => RemoteValue -> f ()
delete rv = command $ "delete " <> var rv

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

data M :: (* -> *) -> * -> * where
 PureM :: a -> M m a
 PrimM :: m a -> M m a
 ApM :: M m (a -> b) -> M m a -> M m b
 BindM :: M m a -> (a -> M m b) -> M m b

instance Functor (M m) where
  fmap f g = pure f <*> g
  
instance Applicative (M m) where
  pure = PureM
  (<*>) = ApM

instance Monad (M m) where
  return = PureM
  (>>=) = BindM
  (>>) = (*>)

evalM :: Monad f => (forall a . m a -> f a) -> M m a -> f a
evalM _ (PureM a) = pure a
evalM f (PrimM p) = f p
evalM f (ApM g h) = evalM f g <*> evalM f h
evalM f (BindM m k) = evalM f m >>=  evalM f . k
