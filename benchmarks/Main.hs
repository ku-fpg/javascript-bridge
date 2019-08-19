{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- stack exec javascript-bridge-benchmarks -- -o benchmarks.html
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
import Control.Monad(foldM)

-- The function we're benchmarking.
fib :: Int -> Int
fib m | m < 0     = error "negative!"
      | otherwise = go m
  where
    go 0 = 0
    go 1 = 1
    go n = go (n-1) + go (n-2)

-- Our benchmark harness.
main = do
        scotty 3000 $ do
          middleware $ start $ \ e -> test e `E.finally`
                       (putStrLn "Finished example")

          get "/" $ do
            html $ mconcat
               [ "<body>"
               ,   "<h1>JavaScript Performance Tests</h1>"
               ,   "<div id='cursor'>"
               ,   "<script>"
               ,     "jsb = new WebSocket('ws://' + location.host + '/');"
               ,     "jsb.onmessage = (evt) => eval(evt.data);"               
               ,   "</script>"
               , "</body>"
               ]
  
          get "/benchmarks.html" $
            file "benchmarks.html"



chain :: Engine -> Int -> Int -> IO Int
chain e n m = send e $ foldM (\ r i -> call m) 0 [1..n]
 where
   call :: (Applicative f, Command f, Procedure f) => Int -> f Int
   call n = sequenceA [ command $ value n | i <- [1..n]] *> (procedure $ value n)
   
test e = do
  send e $ command $ "document.getElementById('cursor').innerHTML +='Starting tests... '"
  defaultMain [
    bgroup ("chain of " ++ show n ++ " element(s)") [
        bench (show m ++ " command(s) in packet") $ whnfIO (chain e n m)
              | m <- [0,10,100,1000]
              ]
         | n <- [1,10,100]
         ]
  send e $ command $ "document.getElementById('cursor').innerHTML +='<a href=\"benchmarks.html\">finished</a>'"
  
