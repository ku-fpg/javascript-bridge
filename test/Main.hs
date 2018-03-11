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
               ,     "jsb.onmessage = function(evt){ "
               ,     "   var reply = function(n,obj) {"
               ,     "       jsb.send(JSON.stringify([n].concat(obj)));"
               ,     "   };"
               ,     "   eval(evt.data);"
               ,     "};"
               ,   "</script>"
               , "</body>"
               ]

example :: JS.Engine IO -> IO ()
example e = do
        print "Starting"
        JS.send e (JS.command "console.log('Hello World!')")
        print "Done command"        
        v <- JS.send e (JS.procedure "1 + 1")
        print ("Done procedure",v)
        v <- JS.send e $ (,)
          <$> JS.procedure "1 + 1"      
          <*  JS.command "console.log(1)"
          <*> JS.procedure "\"Hello\""
          <*  JS.command "console.log(2)"
        print ("Compound",v)
        print "Done"
        return ()
        
