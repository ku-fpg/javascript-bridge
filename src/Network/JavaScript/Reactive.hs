{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Conal's Push-Pull Future. Taken from the paper.
module Network.JavaScript.Reactive
  ( module Network.JavaScript.Reactive,
    ThreadId,
  ) where

import Network.JavaScript.Future

import Control.Applicative
import Control.Monad
import Data.Semigroup
import Control.Concurrent(forkIO,ThreadId)

data Reactive a = a `Stepper` Event a

joinR :: Reactive (Reactive a) -> Reactive a
joinR ((a `Stepper` ur) `Stepper` urr) = a `Stepper` Event u
 -- warning! duplicated urr
 where u = ((`switcher` urr) <$> eFuture ur) `mappend` (joinR <$> eFuture urr)

stepper :: a -> Event a -> Reactive a
stepper = Stepper

switcher :: Reactive a -> Event (Reactive a) -> Reactive a
switcher r e = joinR (r `Stepper` e)

instance Functor Reactive where
  fmap f (a `Stepper` e) = f a `Stepper` fmap f e

instance Applicative Reactive where
  pure a = a `Stepper` Event mempty
  rf@(f `Stepper` Event uf) <*> rx@(x `Stepper` Event ux) = f x `Stepper` Event u
    where u = ((<*> rx) <$> uf) `mappend` ((rf <*>) <$> ux)
instance Monad Reactive where
  return = pure
  r >>= k = joinR (fmap k r)

newtype Event a = Event (Future (Reactive a))
instance Functor Event where
  fmap f (Event m) = Event $ fmap (fmap f) m

instance Applicative Event where
  pure  = Event . return . return
  f <*> x = f `ap` x
instance Monad Event where
  return = pure
  r >>= k = joinE (fmap k r)

eFuture :: Event a -> Future (Reactive a)
eFuture (Event u) = u

joinE :: Event (Event a) -> Event a
joinE (Event u) = Event (u >>= eFuture . g)
 where
   g (e `Stepper` ee) = e `mappend` joinE ee

instance Semigroup (Event a) where
  Event u <> Event v = Event (u `merge` v)

instance Monoid (Event a) where
  mempty = Event mempty
  mappend = (<>) 

merge :: Future (Reactive a) -> Future (Reactive a) -> Future (Reactive a)
merge u v =
    (iFutR (`merge` v) <$> u) `mappend`
    (iFutR (u `merge`) <$> v)
  where
    iFutR f (r `Stepper` Event u') = r `Stepper` Event (f u')

instance Alternative Event where
  empty = mempty
  (<|>) = mappend

instance MonadPlus Event -- use default

forkE :: (a -> IO ()) -> Event a -> IO ThreadId
forkE k = forkIO . sinkE k

sinkE :: (a -> IO ()) -> Event a -> IO ()
sinkE snk (Event f) = do
  (_,r) <- force f
  sinkR snk r

-- wait for an event, then pop it off.
popE :: Event a -> IO (a, Event a)
popE (Event f) = do
  (_,a `Stepper` as) <- force f
  return (a,as)
  
forkR :: (a -> IO ()) -> Reactive a -> IO ThreadId
forkR k = forkIO . sinkR k

sinkR :: (a -> IO ()) -> Reactive a -> IO ()
sinkR snk (a `Stepper` r) = snk a >> sinkE snk r

-- Returns the Event immeduately, but calls the given IO continuation many times.
eventIO :: IO a -> IO (Event a)
eventIO m = Event <$> (futureIO $ do
        a <- m
        reactiveIO a (eventIO m))

reactiveIO :: a -> IO (Event a) -> IO (Reactive a)
reactiveIO a m = (a `Stepper`) <$> m

accumR :: a -> Event (a -> a) -> Reactive a
accumR a e =  a `stepper` (a `accumE` e)

accumE :: a -> Event (a -> a) -> Event a
accumE a (Event e) = Event $ fmap (\ (f `Stepper` e') -> f a `accumR` e') e

-- Return an Event every time a Reactive value changes,
-- where the Event contains the old and new values.
pairReactives :: Reactive a -> Event (a,a)
pairReactives (a `Stepper` Event f) = pairEvents a (Event f)

pairEvents :: a -> Event a -> Event (a,a)
pairEvents a (Event f) = Event $
  fmap (\ (a' `Stepper` e') -> (a,a') `Stepper` pairEvents a' e') f

singletonE :: Future a -> Event a
singletonE = Event . fmap (`Stepper` mempty)

joinMaybes :: MonadPlus m => m (Maybe a) -> m a
joinMaybes m = m >>= maybe mzero return

mapMaybe :: MonadPlus m => (a -> Maybe b) -> m a -> m b
mapMaybe f = joinMaybes . fmap f
