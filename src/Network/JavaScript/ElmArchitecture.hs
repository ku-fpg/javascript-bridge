{-# OPTIONS_GHC -w #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module Network.JavaScript.ElmArchitecture where

import Control.Applicative         ((<|>))
import Data.Aeson                  (Value,ToJSON,toJSON,FromJSON(..),withObject,(.:), Result(..),fromJSON,(.=))
import Data.Maybe
import qualified Data.Aeson as A
import Control.Monad.Trans.State   (State,put,get,runState,evalState,execState)
import Control.Monad.Trans.Writer  (Writer,runWriter,tell, mapWriter)

import Network.JavaScript.Reactive (Event, popE)
import Network.JavaScript.Internal (AF(..),evalAF)
import Network.JavaScript          (sendA, command, call, value, start, Application)
import Data.Text(Text)

class Widget model msg where
  widget :: model -> Remote msg

-- We provide the more general tag-based message, as well as the
-- more uniform model to model version.
instance Widget model msg => Widget [model] (OneOf msg) where
  widget ws = arrayOf (map widget ws)

instance Widget model model => Widget [model] [model] where
  widget ws = flip updateOneOf ws <$> widget ws

updateOneOf :: OneOf model -> [model] -> [model]
updateOneOf (OneOf n w) ws = take n ws ++ [w] ++ drop (n+1) ws
data Remote msg where
  Send       :: ToJSON a => a -> Remote msg
  RecvUnit   :: Remote ()
  RecvDouble :: Remote Double
  RecvText   :: Remote Text
  MapRemote  :: (a -> b) -> Remote a -> Remote b
  Object     :: [Pair msg] -> Remote msg
  Array      :: [Remote msg] -> Remote msg  

instance Functor Remote where
  fmap = MapRemote

send :: ToJSON a => a -> Remote msg
send = Send

class Recv msg where
  recv :: Remote msg

instance Recv () where
  recv = RecvUnit

instance Recv Double where
  recv = RecvDouble

instance Recv Text where
  recv = RecvText

data Pair msg where
  (:=) :: Text -> Remote msg -> Pair msg

-- same as ($)
infixr 0 :=

object :: [Pair msg] -> Remote msg
object = Object

array :: [Remote msg] -> Remote msg
array = Array

wait :: a -> Remote a
wait a = fmap (\ () -> a) recv

data OneOf a = OneOf Int a
  deriving Show

arrayOf :: [Remote msg] -> Remote (OneOf msg)
arrayOf rs = array
  [ OneOf i <$> r
  | (r,i) <- rs `zip` [0..]
  ]

sendRemote :: Remote msg -> State Int Value
sendRemote (Send a) = pure $ toJSON a
sendRemote (RecvUnit) = toJSON <$> alloc 
sendRemote (RecvDouble) = toJSON <$> alloc
sendRemote (RecvText) = toJSON <$> alloc
sendRemote (MapRemote _ r) = sendRemote r
sendRemote (Object pairs) = A.object <$> sequenceA
  [ (lbl .=) <$> sendRemote r
  | lbl := r <- pairs
  ]
sendRemote (Array rs) = toJSON <$> sequenceA (sendRemote <$> rs)

recvRemote :: Remote msg -> WebEvent -> State Int (Maybe msg)
recvRemote (RecvUnit) we = do
  i <- alloc
  case we of
    Click i' | i == i' -> pure (Just ())
    _ -> pure Nothing
recvRemote (RecvDouble) we = do
  i <- alloc
  case we of
    Slide i' v | i == i' -> pure (Just v)
    _ -> pure Nothing
recvRemote (RecvText) we = do
  i <- alloc
  case we of
    Entry i' v | i == i' -> pure (Just v)
    _ -> pure Nothing
recvRemote (Send {}) _ = pure Nothing
recvRemote (Object pairs) we = f <$> sequenceA
    [ recvRemote r we
    | _ := r <- pairs
    ]
  where
    f xs = head $ filter isJust xs ++ [Nothing]
recvRemote (Array rs) we = f <$> sequenceA
    [ recvRemote r we
    | r <- rs
    ]
  where
    f xs = head $ filter isJust xs ++ [Nothing]
recvRemote (MapRemote f r) ev = fmap f <$> recvRemote r ev

alloc :: State Int Int
alloc = do
  s <- get
  put (succ s)
  return s

------------------------------------------------------------------------------

data WebEvent
  = Click Int
  | Slide Int Double
  | Entry Int Text
  deriving Show

instance FromJSON WebEvent where
  parseJSON o = parseClick o <|>
                parseSlide o <|>
                parseEntry o
   where
     parseClick = withObject "Click" $ \v -> Click
        <$> v .: "click"
     parseSlide = withObject "Slide" $ \v -> Slide
        <$> v .: "slide"
        <*> v .: "value"
     parseEntry = withObject "Entry" $ \v -> Entry
        <$> v .: "entry"
        <*> v .: "value"

------------------------------------------------------------------------------
data RuntimeState model = RuntimeState
  { theModel :: model
  , theMsgs  :: Event Value
  , theTick  :: Int
  }

elmArchitecture :: forall model .
                   (Show model, Widget model model)
                => model
                -> Application -> Application
elmArchitecture  m = start $ \ ev e -> do
  print "elmArch"
  let render :: RuntimeState model
             -> IO ()
      render state@RuntimeState{..} = do
        print theModel
        let theView = widget @model @model theModel
        let s0 = 0
        let (json,_) = runState (sendRemote theView) 0
        print ("json",json)
        sendA e $ command $ call "jsb.render" [value (0::Int),value json]
        wait state theView

      wait :: RuntimeState model
           -> Remote model
	   -> IO ()
      wait state@RuntimeState{..} theView = do
        (msg,theMsgs') <- popE theMsgs
        print "waiting for event"
        print msg
        case fromJSON msg :: Result WebEvent of
          Error msg -> do
            print("Error fromJSON msg",msg)
            wait state{theMsgs=theMsgs'} theView
          Success msg' -> do
            print ("got msg",msg')
            case evalState (recvRemote theView msg') 0 of
              Nothing -> do
                print "no match found for event"
                wait state{theMsgs=theMsgs'} theView
              Just theModel' -> 
                render $ RuntimeState { theModel = theModel'
                                      , theMsgs = theMsgs'
                                      , theTick = theTick + 1
                                      }
  render $ RuntimeState { theModel = m
                        , theMsgs = ev
                        , theTick = 0
                        }


------------------------------------------------------------------------------

instance Widget Double Double where
  widget n = object 
      [ "value"  := send n
      , "event"  := recv
      ]

instance Widget Text Text where
  widget n = object 
      [ "value"  := send n
      , "event"  := recv
      ]

