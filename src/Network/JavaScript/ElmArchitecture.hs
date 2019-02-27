{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Network.JavaScript.ElmArchitecture where

import Data.Aeson                  (Value,toJSON,FromJSON(..),withObject,(.:))
import Control.Monad.Trans.State   (State,put,get,runState,execState)
import Control.Monad.Trans.Writer  (Writer,runWriter,tell)

import Network.JavaScript.Reactive (Event)
import Network.JavaScript.Internal (AF(..),evalAF)

data ElmArchitecture effect msg model = ElmArchitecture
  { update  :: msg -> model -> Update effect model
  , view    :: model        -> View msg Value
  , runtime :: effect       -> IO (Event msg)
  }

newtype View msg a = View (AF (Trigger msg) a)
  deriving (Functor,Applicative)

data Trigger msg a where
  Trigger ::  msg  -> Trigger msg Value

trigger :: msg -> View msg Value
trigger = View . PrimAF . Trigger

runView :: Int -> View msg a -> (a,Int)
runView s0 (View m) = runState (evalAF f m) s0
  where
    f :: Trigger msg a -> State Int a
    f (Trigger{}) = do
      s <- get
      put (succ s)
      return $ toJSON s

extractTrigger :: View msg a -> Int -> WebEvent -> Maybe msg
extractTrigger (View m) s0 (Click n) =
    snd $ execState (evalAF f m) (s0,Nothing)
  where
    f :: Trigger msg a -> State (Int,Maybe msg) a
    f (Trigger msg) = do
      (s,i) <- get
      put (succ s,if s == n then Just msg else i)
      return $ toJSON s

instance Show a => Show (View msg a) where
  show = show . fst . runView 1

newtype Update msg a = Update (AF (Effect msg) a)
  deriving (Functor,Applicative)

data Effect msg a where
  Effect :: msg -> Effect msg ()

runUpdate :: Update msg a -> (a,[msg])
runUpdate (Update m) = runWriter (evalAF f m)
  where
    f :: Effect msg a -> Writer [msg] a
    f (Effect msg) = tell [msg]

------------------------------------------------------------------------------

data WebEvent
  = Click Int
  deriving Show

instance FromJSON WebEvent where
  parseJSON = withObject "Click" $ \v -> Click
        <$> v .: "click"

------------------------------------------------------------------------------
-- Toy tester

runElmArchitecture
  :: ElmArchitecture () msg model
  -> model
  -> Thing msg
runElmArchitecture elm@ElmArchitecture{..} m = 
  Thing (view m) $ stepElmArchitecture elm m
      
stepElmArchitecture
  :: ElmArchitecture () msg model
  -> model
  -> msg
  -> Thing msg
stepElmArchitecture elm@ElmArchitecture{..} m msg =
  case runUpdate (update msg m) of
    (m', _) -> runElmArchitecture elm m'

data Thing msg = Thing (View msg Value) (msg -> Thing msg)

step :: msg -> Thing msg -> Thing msg
step msg (Thing _ k) = k msg

-- The key debugging step
instance Show msg => Show (Thing msg) where
  show (Thing doc _k) = show doc

