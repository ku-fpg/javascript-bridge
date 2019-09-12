{-# LANGUAGE OverloadedStrings,KindSignatures, GADTs, ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}

module Network.JavaScript.Internal
  ( -- * JavaScript
    JavaScript(..)
    -- * Commands
  , Command()
  , internalCommand
  , internalConstructor
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
import           Data.Text.Lazy(Text, pack)
import           Data.Text.Lazy.Encoding(encodeUtf8)
import           Data.String

------------------------------------------------------------------------------

newtype JavaScript = JavaScript Text
  deriving Show

instance IsString JavaScript where
  fromString = JavaScript . fromString
  
instance Semigroup JavaScript where
  JavaScript x <> JavaScript y = JavaScript $ x <> y
  
instance Monoid JavaScript where
  mempty = JavaScript mempty
  mappend = (<>)

class Command f where
  internalCommand :: JavaScript -> f ()
  internalConstructor :: JavaScript -> f (RemoteValue a)

class Procedure f where
  internalProcedure :: FromJSON a => JavaScript -> f a
  
-- | The Remote Applicative Packet
newtype Packet a = Packet (AF Primitive a)
  deriving (Functor, Applicative)

-- | The Remote Monad
newtype RemoteMonad a = RemoteMonad (M Primitive a)
  deriving (Functor, Applicative, Monad)

data Primitive :: * -> * where
  Command   :: JavaScript -> Primitive ()
  Procedure :: FromJSON a => JavaScript -> Primitive a
  Constructor :: JavaScript -> Primitive (RemoteValue a)

instance Command Packet where
  internalCommand = Packet . PrimAF . Command  
  internalConstructor = Packet . PrimAF . Constructor

instance Procedure Packet where
  internalProcedure = Packet . PrimAF . Procedure

instance Command RemoteMonad where
  internalCommand = RemoteMonad . PrimM . Command  
  internalConstructor = RemoteMonad . PrimM . Constructor

instance Procedure RemoteMonad where
  internalProcedure = RemoteMonad . PrimM . Procedure

-- A Local handle into a remote value.
newtype RemoteValue a = RemoteValue Int
  deriving (Eq, Ord, Show)

-- Remote values can not be encoded in JSON, but are JavaScript variables.
instance ToJSON (RemoteValue a) where
  toJSON = error "toJSON not supported for RemoteValue"
  toEncoding rv = AI.unsafeToEncoding $ B.fromLazyByteString $ encodeUtf8 txt
    where
      JavaScript txt = var rv
      
-- | generate the text for a RemoteValue. They can be used as assignment
--   targets as well, but exposes the JavaScript scoping semantics.
var :: RemoteValue a -> JavaScript
var (RemoteValue n) = JavaScript $ "jsb.rs[" <> pack (show n) <> "]"

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

concatAF :: (forall x . m x -> Maybe b) -> AF m a -> [b]
concatAF _ (PureAF _) = []
concatAF f (PrimAF p) = case f p of
  Nothing -> []
  Just r -> [r]
concatAF f (ApAF m1 m2) = concatAF f m1 ++ concatAF f m2

evalAF :: Applicative f => (forall x . m x -> f x) -> AF m a -> f a
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

evalM :: Monad f => (forall x . m x -> f x) -> M m a -> f a
evalM _ (PureM a) = pure a
evalM f (PrimM p) = f p
evalM f (ApM g h) = evalM f g <*> evalM f h
evalM f (BindM m k) = evalM f m >>=  evalM f . k
