{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Implements Conal's Push-Pull Future, but using STMs.
module Network.JavaScript.Future where

import Control.Concurrent
import Control.Concurrent.STM
import Data.Semigroup
import System.IO.Unsafe

data Future a
  = Future (STM (TimeStamp,a))
  | NoFuture
-- We could optimize Future, providing a
--  * pure constructor (always has happened)
--  * mempty constructor (never happens)

{-# NOINLINE timeSupply #-}
-- We intentually increment the epoch for the first stored value.
timeSupply :: TVar TimeStamp
timeSupply = unsafePerformIO $ newTVarIO $ succ epoch 

epoch :: TimeStamp
epoch = TimeStamp 0

newtype TimeStamp = TimeStamp Int
  deriving (Eq,Ord,Show,Enum)
 
instance Functor Future where
  fmap f (Future m) = Future $ fmap (\ (t,a) -> (t,f a)) m
  fmap _ NoFuture = NoFuture

instance Applicative Future where
  pure a = Future $ pure (epoch,a)
  NoFuture <*> _ = NoFuture
  _ <*> NoFuture = NoFuture
  Future m1 <*> Future m2 = Future $ do
    (t1,r1) <- m1
    (t2,r2) <- m2
    return $ (t1 `max` t2,r1 r2)

instance Monad Future where
  return = pure
  NoFuture >>= _ = NoFuture
  Future m1 >>= k = Future $ do
    (t1,r1) <- m1
    case k r1 of
      Future m2 -> do
        (t2,r2) <- m2
        return (t1 `max` t2,r2)
      NoFuture -> retry
        
instance Semigroup (Future a) where
  -- Note: is it not possible to get a retry
  -- for r1 or r2, then later resolve to an *earlier* time,
  -- because of the global timeStamp namesupply.
  NoFuture <> f = f
  f <> NoFuture = f
  Future m1 <> Future m2 = Future $
     (do r1@(t1,_) <- m1
         ((\ r2@(t2,_) -> if t1 <= t2 then r1 else r2) <$> m2)
           `orElse` pure r1
     ) `orElse` m2

instance Monoid (Future a) where
  mempty = NoFuture
  mappend = (<>)

force :: Future a -> IO (TimeStamp,a)
force = atomically . forceSTM

forceSTM :: Future a -> STM (TimeStamp,a)
forceSTM NoFuture = retry
forceSTM (Future m) = do
  (t,r) <- m
  return (t,r)

future :: STM (TimeStamp,a) -> Future a
future = Future

-- Returns the Future immeduately, and only calls the given IO continuation *once*.
futureIO :: IO a -> IO (Future a)
futureIO m = do
  v <- atomically newEmptyTMVar
  _ <- forkIO $ do
        a <- m
        atomically $ do
          tm <- readTVar timeSupply
          writeTVar timeSupply $ succ tm
          putTMVar v (tm,a)
  return (Future $ readTMVar v)


