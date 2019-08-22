{-# LANGUAGE OverloadedStrings,KindSignatures, GADTs, ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -w #-}
module Network.JavaScript
  ( -- * Remote Applicative Packets of JavaScript
    Command()
  , Procedure()
  , JavaScript(..)
  , Packet
  , RemoteMonad
  , command
  , procedure
  , constructor
  , sync
  , localize
  , unlocalize
    -- * sending Packets
  , send
  , sendA
  , sendE
    -- * Remote and Local Values
  , RemoteValue
  , delete
    -- * Text builders
  , var
  , value
  , call
  , number
  , string
    -- * Events
  , JavaScriptException(..)
  , addListener
  , listen
  , readEventChan
  -- * Web services
  , start
  , Engine
  , Application
  ) where

import Control.Applicative((<|>),liftA2)
import Control.Exception(Exception)
import Control.Exception as Exception
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy (Text)
import qualified Network.Wai.Handler.WebSockets as WS
import Network.Wai (Application)
import qualified Network.WebSockets as WS
import Control.Monad.Trans.State.Strict

import Control.Concurrent (forkIO)
import Control.Exception (try, SomeException)
import Control.Monad (forever)
import Control.Concurrent.STM
import Data.Aeson ( Value(..), decode', FromJSON(..),withObject,(.:)
                  , ToJSON(..), encode, Result(..), fromJSON)
import Data.Text.Lazy.Encoding(decodeUtf8,encodeUtf8)
import qualified Data.Aeson.Encoding.Internal as AI
import qualified Data.Binary.Builder as B
import qualified Data.IntMap.Strict as IM


import Network.JavaScript.Internal
import Network.JavaScript.Services

------------------------------------------------------------------------------

-- | 'command' statement to execute in JavaScript. ';' is not needed as a terminator.
--   Should never throw an exception, which may be reported to console.log.
command :: Command f => JavaScript -> f ()
command = internalCommand
  
-- | 'constructor' expression to execute in JavaScript. ';' is not needed as a terminator.
--   Should never throw an exception, but any exceptions are returned to the 'send'
--   as Haskell exceptions.
--
--   The value returned in not returned to Haskell. Instead, a handle is returned,
--   that can be used to access the remote value. Examples of remote values include
--   objects that can not be serialized, or values that are too large to serialize.
--
--   The first type argument is the phantom type of the 'RemoteValue', so that
--   type application can be used to specify the type.
constructor :: forall a f . Command f => JavaScript -> f (RemoteValue a)
constructor = internalConstructor

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
procedure :: forall a f . (Procedure f, FromJSON a) => JavaScript -> f a
procedure = internalProcedure

as :: (FromJSON a, Procedure g) => (forall f . Command f => f (RemoteValue a)) -> g a
as (Packet (PrimAF (Constructor cmd))) = procedure cmd
-- is@Int $ command "fooo"

-- | A sync will always flush the send queue.
sync :: (Monad f, Procedure f) => f ()
sync = procedure "null" >>= \ Null -> return ()

------------------------------------------------------------------------------

send :: Engine -> RemoteMonad a -> IO a
send e p = do
  r <- sendE e p
  case r of
    Right a -> return a
    Left err -> throwIO $ JavaScriptException err

data JavaScriptException = JavaScriptException Value
    deriving (Show,Eq)

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
  CommandStmt   :: JavaScript -> Stmt ()
  ProcedureStmt :: FromJSON a => Int -> JavaScript -> Stmt a
  ConstructorStmt :: RemoteValue a -> JavaScript -> Stmt (RemoteValue a)

deriving instance Show (Stmt a)
  
prepareStmtA :: Monad f => f Int -> AF Primitive a -> f (AF Stmt a)
prepareStmtA _  (PureAF a) = pure (pure a)
prepareStmtA ug (PrimAF p) = PrimAF <$> prepareStmt ug p
prepareStmtA ug (ApAF g h) = ApAF <$> prepareStmtA ug g <*> prepareStmtA ug h

prepareStmt :: Monad f => f Int -> Primitive a -> f (Stmt a)
prepareStmt ug (Command stmt)     = pure $ CommandStmt stmt
prepareStmt ug (Procedure stmt)   = ug >>= \ i -> pure $ ProcedureStmt i stmt
prepareStmt ug (Constructor stmt) = ug >>= \ i -> pure $ ConstructorStmt (RemoteValue i) stmt
   
showStmtA :: AF Stmt a -> JavaScript
showStmtA stmts = JavaScript
                $ LT.concat [ js | JavaScript js <- concatAF (return . showStmt) stmts ]

showStmt :: Stmt a -> JavaScript
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
    (v:vs') -> put vs' >> case fromJSON v of
      Error _ -> fail "can not parse result"
      Success r -> return r
    _ -> fail "not enough values"
evalStmt (ConstructorStmt c _) = pure c

sendStmtA :: Engine -> AF Stmt a -> IO (Either Value a)
sendStmtA e (PureAF a) = return (pure a)
sendStmtA e af
    | null assignments = do
        sendJavaScript e $ showStmtA af
        return $ case evalStmtA af [] of
            Nothing -> error "internal failure"
            Just r -> Right r        
    | otherwise = do
        nonce <- genNonce e
        sendJavaScript e $ catchMe nonce $ showStmtA af
        theReply <- replyBox e nonce
        case theReply of
          Right replies -> return $ case evalStmtA af replies of
            Nothing -> error "internal failure"
            Just r -> Right r
          Left err -> return $ Left err
        
  where
    catchMe :: Int -> JavaScript -> JavaScript
    catchMe nonce txt =
      "try{" <> txt
             <> "}catch(err){jsb.error(" <> JavaScript (LT.pack (show nonce))
             <> ",err);};" <>
      reply nonce <> ";"

    assignments :: [Int]
    assignments = concatAF findAssign af

    findAssign :: Stmt a -> Maybe Int
    findAssign (ProcedureStmt i _) = Just i
    findAssign _ = Nothing
    
    -- generate the call to reply (as a final command)
    reply :: Int -> JavaScript
    reply n = JavaScript $
      "jsb.reply(" <> LT.intercalate ","
      [ LT.pack (show n)
      , "[" <> LT.intercalate "," [ x | JavaScript x <- map procVar assignments] <> "]"
      ] <> ")"

-- TODO: Consider a wrapper around this Int
procVar :: Int -> JavaScript
procVar n = JavaScript $ "v" <> LT.pack (show n)

------------------------------------------------------------------------------

-- | 'delete' a remote value.
delete :: Command f => RemoteValue a -> f ()
delete rv = command $ "delete " <> var rv

-- | 'localize' brings a remote value into Haskell.
localize :: Procedure f => RemoteValue a -> f Value
localize = procedure . var

-- | 'unlocalize' sends a local value to JavaScript.
unlocalize :: Command f => Value -> f (RemoteValue a)
unlocalize = constructor . value

-- | Generate the 'Text' for a JavaScript value, including 'RemoteValue'.
value :: ToJSON v => v -> JavaScript
value = JavaScript . decodeUtf8 . encode

-- | Generate JavaScript number
number :: Double -> JavaScript
number = value

-- | Generate (quoted) JavaScript string
string :: Text -> JavaScript
string = value

-- | generate a function call
call :: JavaScript -> [JavaScript] -> JavaScript
call fn args = fn <> "(" <> JavaScript (LT.intercalate "," [ js | JavaScript js <- args ]) <> ")"
