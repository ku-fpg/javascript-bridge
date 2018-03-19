{-# LANGUAGE OverloadedStrings #-}


import qualified Network.JavaScript as JS
import Web.Scotty

main :: IO ()
main = do
        scotty 3000 $ do
          middleware $ JS.start example

          get "/" $ do
            html $ mconcat
               [ "<body>"
               ,   "<h1>Hello!</h1>"
               ,   "<script>"
               ,     "jsb = new WebSocket('ws://localhost:3000/');"
               ,     "jsb.onmessage = function(evt){ "
               ,     "   var reply = function(n,obj) {"
               ,     "       Promise.all(obj).then(function(obj){"
               ,     "         jsb.send(JSON.stringify([n].concat(obj)));"
               ,     "       });"
               ,     "   };"
               ,     "   eval('(function(){' + evt.data + '})()');"
               ,     "};"
               ,   "</script>"
               , "</body>"
               ]

example :: JS.Engine IO -> IO ()
example e = do
        JS.addListener e print
        putStrLn "Starting"
        JS.send e (JS.command "console.log('Hello World!')")
        putStrLn "Done command"
        v1 <- JS.send e (JS.procedure "1 + 1")
        print ("Done procedure"::String,v1)
        v2 <- JS.send e $ (,)
           <$> JS.procedure "1 + 1"
           <*  JS.command "console.log(1)"
           <*> JS.procedure "\"Hello\""
           <*  JS.command "console.log(2)"
        print ("Compound"::String,v2)
        v3 <- JS.send e $
              JS.procedure "new Promise(function(good,bad) { good('Hello') })"
        print ("Promise"::String,v3)
        v4 <- JS.send e $ (,,)
           <$> JS.procedure "new Promise(function(good,bad) { good('Hello') })"
           <*  JS.command "console.log(1)"
           <*> JS.procedure "\"Hello\""
           <*  JS.command "console.log(2)"
           <*> JS.procedure "new Promise(function(good,bad) { good('World') })"
        print ("All"::String,v4)
        putStrLn "Done"
        return ()

