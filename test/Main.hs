{-# LANGUAGE OverloadedStrings #-}


import qualified Network.JavaScript as JS
import Web.Scotty

main = do
        scotty 3000 $ do
          middleware $ JS.start print example

          get "/" $ do
            html $ mconcat
               [ "<body>"
               ,   "<h1>Hello!</h1>"
               ,   "<script>"
               ,     "jsb = new WebSocket('ws://localhost:3000/');"
               ,     "jsb.onmessage = function(evt){ eval(evt.data);};"
               ,   "</script>"
               , "</body>"
               ]

example :: JS.Engine -> IO ()
example e = do
        print "Starting"
        JS.sendCommand e (JS.Command "alert('Hello World!')")
        print "Hacking"
        JS.hack e
        print "Done"
        return ()
        