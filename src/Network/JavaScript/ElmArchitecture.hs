{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Network.JavaScript.ElmArchitecture where

import Control.Applicative         ((<|>))
import Data.Aeson                  (Value,toJSON,FromJSON(..),withObject,(.:), Result(..),fromJSON)
import Control.Monad.Trans.State   (State,put,get,runState,execState)
import Control.Monad.Trans.Writer  (Writer,runWriter,tell)

import Network.JavaScript.Reactive (Event, popE)
import Network.JavaScript.Internal (AF(..),evalAF)
import Network.JavaScript          (sendA, command, call, value, start, Application)


data ElmArchitecture effect msg model = ElmArchitecture
  { update  :: msg -> model -> Update effect model
  , view    :: model        -> View msg Value
  , runtime :: effect       -> IO (Event msg)
  }

newtype View msg a = View (AF (Trigger msg) a)
  deriving (Functor,Applicative)

data Trigger msg a where
  Trigger ::  msg  -> Trigger msg Value
  SliderTrigger :: (Double -> msg) -> Trigger msg Value

trigger :: msg -> View msg Value
trigger = View . PrimAF . Trigger

slider :: (Double -> msg) -> View msg Value
slider =  View . PrimAF . SliderTrigger

runView :: Int -> View msg a -> (a,Int)
runView s0 (View m) = runState (evalAF f m) s0
  where
    f :: Trigger msg a -> State Int a
    f (Trigger{}) = do
      s <- get
      put (succ s)
      return $ toJSON s
    f (SliderTrigger{}) = do
      s <- get
      put (succ s)
      return $ toJSON s

extractTrigger :: View msg a -> Int -> WebEvent -> Maybe msg
extractTrigger (View m) s0 webEvent =
    snd $ execState (evalAF f m) (s0,Nothing)
  where
    f :: Trigger msg a -> State (Int,Maybe msg) a
    f (Trigger msg) = do
      (s,i) <- get
      put (succ s,case webEvent of
                    Click n | s == n -> Just msg
                    _ -> i)
      return $ toJSON s
    f (SliderTrigger k) = do
      (s,i) <- get
      put (succ s,case webEvent of
                    Slide n v | s == n -> Just $ k v
                    _ -> i)
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


------------------------------------------------------------------------------
data RuntimeState msg model = RuntimeState
  { theModel :: model
  , theMsgs  :: Event Value
  , theTick  :: Int
  }

elmArchitecture :: forall effect msg model f . (Show effect, Show msg)
                => ElmArchitecture effect msg model
                -> model
                -> Application -> Application
elmArchitecture ea m = start $ \ ev e -> do
  print "elmArch"
  let render :: RuntimeState msg model
             -> IO ()
      render state@RuntimeState{..} = do
        let theView = view ea theModel
        let s0 = 0
        let (json,_) = runView 0 theView
        print json
        sendA e $ command $ call "jsb.render" [value (0::Int),value json]
        wait state theView

      wait :: RuntimeState msg model
           -> View msg Value
	   -> IO ()
      wait state@RuntimeState{..} theView = do
        (msg,theMsgs') <- popE theMsgs
        print msg
        case fromJSON msg of
          Error{} -> wait state{theMsgs=theMsgs'} theView
          Success msg' -> do
            print msg'
            case extractTrigger theView 0 msg' of
              Nothing -> wait state{theMsgs=theMsgs'} theView
              Just msg'' -> do
                print msg''
                let (theModel', effects) = runUpdate $ update ea msg'' theModel
                print effects
                render $ RuntimeState { theModel = theModel'
                                      , theMsgs = theMsgs'
                                      , theTick = theTick + 1
                                      }
  render $ RuntimeState { theModel = m
                        , theMsgs = ev
                        , theTick = 0
                        }
