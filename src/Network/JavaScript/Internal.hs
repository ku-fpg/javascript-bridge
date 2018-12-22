{-# LANGUAGE OverloadedStrings,KindSignatures, GADTs, ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -v #-}
module Network.JavaScript.Internal
  ( -- * Remote Applicative Packets of JavaScript
    Command(..)
  , Procedure(..)
  , Packet(..)
  , RemoteMonad(..)
  , Primitive(..)
  , RemoteValue(..)
  , AF(..)
  , M(..)
  , evalAF
  , concatAF
  , evalM
  ) where

import           Data.Text.Lazy(Text)

import Data.Aeson (Value(..), FromJSON(..))

------------------------------------------------------------------------------

class Command f where
  -- | 'command' statement to execute in JavaScript. ';' is not needed as a terminator.
  --   Should never throw an exception, which may be reported to console.log.
  command :: Text -> f ()
  -- | 'constructor' expression to execute in JavaScript. ';' is not needed as a terminator.
  --   Should never throw an exception, but any exceptions are returned to the 'send'
  --   as Haskell exceptions.
  --
  --   The value returned in not returned to Haskell. Instead, a handle is returned,
  --   that can be used to access the remote value. Examples of remote values include
  --   objects that can not be serialized, or values that are too large to serialize.
  constructor :: Text -> f (RemoteValue a)

  -- | a 'function' takes a Haskell function, and converts
  --   it into a JavaScript function. This can be used to 
  --   generate first-class functions, for passing as arguments.
  --   TODO: generalize to Monad.
  function :: (forall g . (Command g, Applicative g) => RemoteValue (a -> IO b) -> RemoteValue a -> g (RemoteValue b))

           -> f (RemoteValue (a -> IO b))

--  continuation :: (forall g . (Command g, Procedure g, Monad g) => RemoteValue a -> g ())
--               -> f (RemoteValue (a -> IO ()))

class Procedure f where
  proc :: FromJSON a => Text -> f a
  
-- | Deep embedding of an applicative packet
newtype Packet a = Packet (AF Primitive a)
  deriving (Functor, Applicative)

-- | Deep embedding of an applicative packet
newtype RemoteMonad a = RemoteMonad (M Primitive a)
  deriving (Functor, Applicative, Monad)

data Primitive :: * -> * where
  Command   :: Text -> Primitive ()
  Procedure :: Text -> Primitive Value
  Procedure' :: FromJSON a => Text -> Primitive a
  Constructor :: Text -> Primitive (RemoteValue a)
  Function :: (RemoteValue (a -> IO b) -> RemoteValue a -> Packet (RemoteValue b))
           -> Primitive (RemoteValue (a -> IO b))

instance Command Packet where
  command = Packet . PrimAF . Command  
  constructor = Packet . PrimAF . Constructor
  function k = Packet $ PrimAF $ Function k

instance Procedure Packet where
  proc = Packet . PrimAF . Procedure'

instance Command RemoteMonad where
  command = RemoteMonad . PrimM . Command  
  constructor = RemoteMonad . PrimM . Constructor
  function k = RemoteMonad $ PrimM $ Function k

instance Procedure RemoteMonad where
  proc = RemoteMonad . PrimM . Procedure'

-- A Local handle into a remote value.
data RemoteValue a = RemoteValue Int
                   | RemoteArgument Int
  deriving (Eq, Ord, Show)

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
