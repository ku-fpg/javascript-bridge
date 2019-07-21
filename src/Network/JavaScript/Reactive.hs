{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}


-- Conal's Push-Pull Future. Taken from the paper.
module Network.JavaScript.Reactive
  ( Event
  , event
  , forkE
  , sinkE
  , waitE
  , eventIO
  , ThreadId
  ) where

import Network.JavaScript.Future

import Data.Semigroup
import Control.Concurrent(forkIO,ThreadId)

newtype Event a = Event (Future (a, Event a))

event :: a -> Event a
event a = Event $ pure (a,mempty)

instance Functor Event where
  fmap f (Event m) = Event $ fmap (\ (a,e) -> (f a, fmap f e)) m

instance Semigroup (Event a) where
  Event u <> Event v = Event (u `merge` v)

instance Monoid (Event a) where
  mempty = Event mempty
  mappend = (<>) 

merge :: forall a . Future (a, Event a) -> Future (a, Event a) -> Future (a, Event a)
merge u v =
    (iFutR (`merge` v) <$> u) `mappend`
    (iFutR (u `merge`) <$> v)
  where
    iFutR :: (Future (a, Event a) -> Future (a, Event a)) -> (a,Event a) -> (a, Event a)
    iFutR f (r,Event u') = (r, Event (f u'))

forkE :: (a -> IO ()) -> Event a -> IO ThreadId
forkE k = forkIO . sinkE k

sinkE :: (a -> IO ()) -> Event a -> IO ()
sinkE snk (Event f) = do
  (_,(a,as)) <- force f
  snk a
  sinkE snk as

-- wait for an event, then pop the value,
-- and the rest of the event.
waitE :: Event a -> IO (a, Event a)
waitE (Event f) = snd <$> force f

-- Returns the Event immeduately, but calls the given IO continuation many times.
eventIO :: IO a -> IO (Event a)
eventIO m = Event <$> (futureIO $ do
        a <- m
        (a,) <$> eventIO m)
