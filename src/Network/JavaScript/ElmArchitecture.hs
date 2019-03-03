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

-- Widgets can not have effects
class Widget model where
  type Message model
  update   :: (msg ~ Message model) => msg    -> model -> model
  view     :: (msg ~ Message model) => model  -> Remote msg

  default update :: (Applet model, msg ~ Message model) => msg -> model -> model    
  update  msg model = fst $ runWriter $ applet msg model

instance Widget model => Widget [model] where
  type Message [model] = OneOf (Message model)
  update (OneOf n w) ws = take n ws ++ [update w (ws !! n)] ++ drop (n+1) ws
  view ws = arrayOf (map view ws)

-- Applets can have external effect
class Widget model => Applet model where
  applet  :: (msg ~ Message model) => msg -> model -> Writer (IO (Event msg)) model
  applet msg model = pure $ update msg model
  
instance Applet model => Applet [model] where
  applet (OneOf n w) ws =
      (\ w' -> take n ws ++ [w'] ++ drop (n+1) ws) <$>
        mapWriter (\ (a,w) -> (a,fmap (fmap (OneOf n)) w)) (applet w (ws !! n))
      
data Remote msg where
  Send       :: ToJSON a => a -> Remote msg
  RecvUnit   :: Remote ()
  RecvDouble :: Remote Double
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

instance Recv Double  where
  recv = RecvDouble

data Pair msg where
  (:=) :: Text -> Remote msg -> Pair msg

infix 0 :=

object :: [Pair msg] -> Remote msg
object = Object

array :: [Remote msg] -> Remote msg
array = Array

recv_ :: Remote ()
recv_ = recv

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
  | Slide Int Double
  deriving Show

instance FromJSON WebEvent where
  parseJSON o = parseClick o <|>
                parseSlide o
   where
     parseClick = withObject "Click" $ \v -> Click
        <$> v .: "click"
     parseSlide = withObject "Slide" $ \v -> Slide
        <$> v .: "slide"
        <*> v .: "value"

------------------------------------------------------------------------------
data RuntimeState model = RuntimeState
  { theModel :: model
  , theMsgs  :: Event Value
  , theTick  :: Int
  }

elmArchitecture :: forall msg model .
                   (Widget model, msg ~ Message model, Show msg)
                => model
                -> Application -> Application
elmArchitecture  m = start $ \ ev e -> do
  print "elmArch"
  let render :: RuntimeState model
             -> IO ()
      render state@RuntimeState{..} = do
        let theView = view theModel
        let s0 = 0
        let (json,_) = runState (sendRemote theView) 0
        print json
        sendA e $ command $ call "jsb.render" [value (0::Int),value json]
        wait state theView

      wait :: RuntimeState model
           -> Remote msg
	   -> IO ()
      wait state@RuntimeState{..} theView = do
        (msg,theMsgs') <- popE theMsgs
        print msg
        case fromJSON msg of
          Error{} -> wait state{theMsgs=theMsgs'} theView
          Success msg' -> do
            print msg'
            case evalState (recvRemote theView msg') 0 of
              Nothing -> wait state{theMsgs=theMsgs'} theView
              Just msg'' -> do
                print msg''
                let theModel' = update msg'' theModel
                render $ RuntimeState { theModel = theModel'
                                      , theMsgs = theMsgs'
                                      , theTick = theTick + 1
                                      }
  render $ RuntimeState { theModel = m
                        , theMsgs = ev
                        , theTick = 0
                        }

