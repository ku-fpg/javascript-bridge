{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import Criterion.Main

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad (forever)
import Data.Aeson
import Data.Monoid((<>))
import qualified Data.Text.Lazy as T
import Network.JavaScript
import System.Exit
import Web.Scotty


-- Our benchmark harness.
main = do
        lock <- newEmptyMVar

        _ <- forkIO $ scotty 3000 $ do
          middleware $ start $ \ e -> test e `E.finally`
                       (do putMVar lock ()
                           putStrLn "Finished example")


          get "/" $ do
            html $ mconcat
               [ "<body>"
               ,   "<h1>JavaScript Performance Tests</h1>"
               ,   "<div id='cursor'>"
                 -- Include this code in your page
               ,   "<script>"
               ,     "jsb = new WebSocket('ws://' + location.host + '/');"
               ,     "jsb.onmessage = (evt) => eval(evt.data);"               
               ,   "</script>"
               , "</body>"
               ]

        takeMVar lock

call :: Engine -> Int -> IO (Result Int)
call e n = fromJSON <$> send e (sequenceA [ command $ val n | i <- [1..n]] *> (procedure $ val n))

test e = 
  defaultMain [ bench (show n) $ whnfIO (call e n)
              | n <- [0,10,100,1000,10000]
              ]
  
