{-# LANGUAGE OverloadedStrings,KindSignatures, GADTs, ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -w #-}
module Network.JavaScript.Internal
  ( -- * Commands
    Command()
  , internalCommand
  , internalConstructor
  , internalFunction
    -- * Procedures
  , Procedure()
  , internalProcedure
    -- * Primitives and (Remote) Values
  , Primitive(..)
  , RemoteValue(..)
  , var
    -- * (Applicative) Packets
  , Packet(..)
  , AF(..)
  , RemoteMonad(..)
  , evalAF
  , concatAF
    -- * Monads
  , M(..)
  , evalM
  ) where

import           Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Data.Aeson.Encoding.Internal as AI
import qualified Data.Binary.Builder as B
import           Data.Monoid ((<>))
import           Data.Text.Lazy(Text, pack)
import           Data.Text.Lazy.Encoding(encodeUtf8)


------------------------------------------------------------------------------

class Command f where
  internalCommand :: Text -> f ()
  internalConstructor :: Text -> f (RemoteValue a)
  internalFunction :: (forall g . (Command g, Applicative g) => RemoteValue (a -> IO b) -> RemoteValue a -> g (RemoteValue b))

           -> f (RemoteValue (a -> IO b))
--  aContinuation :: (forall g . (Command g, Procedure g, Monad g) => RemoteValue a -> g ())
--               -> f (RemoteValue (a -> IO ()))

class Procedure f where
  internalProcedure :: FromJSON a => Text -> f a
  
-- | Deep embedding of an applicative packet
newtype Packet a = Packet (AF Primitive a)
  deriving (Functor, Applicative)

-- | Deep embedding of an applicative packet
newtype RemoteMonad a = RemoteMonad (M Primitive a)
  deriving (Functor, Applicative, Monad)

data Primitive :: * -> * where
  Command   :: Text -> Primitive ()
  Procedure :: FromJSON a => Text -> Primitive a
  Constructor :: Text -> Primitive (RemoteValue a)
  Function :: (RemoteValue (a -> IO b) -> RemoteValue a -> Packet (RemoteValue b))
           -> Primitive (RemoteValue (a -> IO b))

instance Command Packet where
  internalCommand = Packet . PrimAF . Command  
  internalConstructor = Packet . PrimAF . Constructor
  internalFunction k = Packet $ PrimAF $ Function k

instance Procedure Packet where
  internalProcedure = Packet . PrimAF . Procedure

instance Command RemoteMonad where
  internalCommand = RemoteMonad . PrimM . Command  
  internalConstructor = RemoteMonad . PrimM . Constructor
  internalFunction k = RemoteMonad $ PrimM $ Function k

instance Procedure RemoteMonad where
  internalProcedure = RemoteMonad . PrimM . Procedure

-- A Local handle into a remote value.
data RemoteValue a = RemoteValue Int
                   | RemoteArgument Int
  deriving (Eq, Ord, Show)

-- Remote values can not be encoded in JSON, but are JavaScript variables.
instance ToJSON (RemoteValue a) where
  toJSON = error "toJSON not supported for RemoteValue"
  toEncoding = AI.unsafeToEncoding . B.fromLazyByteString . encodeUtf8 . var

-- | generate the text for a RemoteValue. They can be used as assignment
--   targets as well, but exposes the JavaScript scoping semantics.
var :: RemoteValue a -> Text
var (RemoteValue n) = "jsb.rs[" <> pack (show n) <> "]"
var (RemoteArgument n) = "a" <> pack (show n)

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
