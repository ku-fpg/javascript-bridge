{-# LANGUAGE OverloadedStrings,KindSignatures, GADTs, ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
module Network.JavaScript
  ( -- * Remote Applicative Packets of JavaScript
    Remote
  , Packet
  , command
  , procedure
  , constructor
  , function
    -- * sending Packets
  , send
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

class RemoteProcedure f where
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

-- | Deep embedding of an applicative packet
newtype Packet a = Packet (AF Primitive a)
  deriving (Functor, Applicative)

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

------------------------------------------------------------------------------

send :: Engine -> Packet a -> IO a
send e p = do
  r <- sendE e p
  case r of
    Right a -> return a
    Left err -> throwIO $ JavaScriptException err

data JavaScriptException = JavaScriptException Value
    deriving Show

instance Exception JavaScriptException

sendE :: Engine -> Packet a -> IO (Either Value a)
sendE e@Engine{..} (Packet af) = prepareStmtA genNonce af >>= sendStmtA e

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

