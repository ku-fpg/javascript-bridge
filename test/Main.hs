{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}


import Control.Applicative
import Data.Aeson
import Data.Monoid((<>))
import qualified Data.Text.Lazy as T
import qualified Network.JavaScript as JS
import System.Exit
import Web.Scotty

main :: IO ()
main = do
        scotty 3000 $ do
          middleware $ JS.start example

          get "/" $ do
            html $ mconcat
               [ "<body>"
               ,   "<h1>JavaScript Bridge Tests</h1>"
               ,    "<h3>Sending Commands</h3>"
               ,    "<ul><li id='send-command'><i style='color: #ff0000'>waiting for send $ command</i></li></ul>"
               ,   "<div id='cursor'>"
               ,   "<script>"
               ,     "jsb = new WebSocket('ws://' + location.host + '/');"
               ,     "jsb.onmessage = function(evt){ "
               ,     "   var reply = function(n,obj) {"
               ,     "       Promise.all(obj).then(function(obj){"
               ,     "         jsb.send(JSON.stringify([n].concat(obj)));"
               ,     "       });"
               ,     "   };"
               ,     "   if (true || debug) { console.log('eval',evt.data); }"
               ,     "   eval('(function(){' + evt.data + '})()');"
               ,     "};"
               ,   "</script>"
               , "</body>"
               ]



write :: JS.Engine IO -> String -> IO ()
write e txt = JS.send e $ JS.command ("document.getElementById('cursor').innerHTML += " <> T.pack (show txt))

jsWriteTo :: String -> String -> JS.Packet ()
jsWriteTo i txt = JS.command ("document.getElementById('" <> T.pack i <> "').innerHTML += " <> T.pack (show txt))

writeTo:: JS.Engine IO -> String -> String -> IO ()
writeTo e i txt = JS.send e $ JS.command ("document.getElementById('" <> T.pack i <> "').innerHTML = " <> T.pack (show txt))

scroll :: JS.Engine IO -> String -> IO ()
scroll e i = JS.send e $ JS.command $ "document.getElementById('" <> T.pack i <> "').scrollIntoView({behavior: 'smooth', block: 'end'})"


assert :: (Eq a, Show a) => JS.Engine IO -> Result a -> a -> IO ()
assert e (Error er) _ = do
  write e $ "<ul><li><span style='color: red'>Failed to parse result: " ++ show er ++ "</span></ul></li>"
  exitFailure
assert e (Success n) g
   | n == g = do
       write e $ "<ul><li>result parsed as " ++ show n ++ ", and is correct</li></ul>"       
   | otherwise = do
       write e $ "<ul><li><span style='color: red'>unexpected result : '" ++ show n ++ "'</span></li></ul>"
       exitFailure

example :: JS.Engine IO -> IO ()
example e = do
        JS.addListener e print

        -- First test of send command
        writeTo e "send-command" "send $ command ... works"

        scroll e "cursor"

        -- We assume send command works; and test procedures
        write e "<h3>Sending Procedures</h3>"
        v1 :: Value <- JS.send e (JS.procedure "1 + 1")
        write e "<ul><li>send $ procedure (1+1) works</li></ul>"

        assert e (fromJSON v1) (2 :: Int)

        v2 :: Value <- JS.send e (JS.procedure "'Hello'")
        write e "<ul><li>send $ procedure 'Hello' works</li></ul>"

        assert e (fromJSON v2) ("Hello" :: String)

        v3 :: Value <- JS.send e (JS.procedure "[true,false]")
        write e "<ul><li>send $ procedure [true,false] works</li></ul>"

        assert e (fromJSON v3) [True,False]

        scroll e "cursor"
        
        write e "<h3>Sending Combine Commands</h3>"
        JS.send e $ 
             jsWriteTo "cursor" "<ul id='combine-commands'></ul>" 
          <* jsWriteTo "combine-commands" "<li>command 1</li>"        
          <* jsWriteTo "combine-commands" "<li>command 2</li>"        

        scroll e "cursor"

        write e "<h3>Sending Combine Procedures</h3>"
        v4 :: Result (Int,String,Bool) <- JS.send e $ liftA3 (,,)
          <$> (fromJSON <$> JS.procedure "1 + 1")
          <*> (fromJSON <$> JS.procedure "'Hello'")
          <*> (fromJSON <$> JS.procedure "true")
        JS.send e $ 
             jsWriteTo "cursor" "<ul id='combine-procs'></ul>" 
          <* jsWriteTo "combine-procs" "<li>1+1</li>"        
          <* jsWriteTo "combine-procs" "<li>'Hello'</li>"        
          <* jsWriteTo "combine-procs" "<li>'True'</li>"
  
        assert e v4 (2,"Hello",True)
        scroll e "cursor"

        write e "<h3>Sending Combine Commands and Procedures</h3>"

        v5 :: Result (Int,String,Bool) <- JS.send e $ liftA3 (,,)
          <$> (fromJSON <$> JS.procedure "1 + 1")
          <*> (fromJSON <$> JS.procedure "'Hello'")
          <*> (fromJSON <$> JS.procedure "true")
          <* jsWriteTo "cursor" "<ul id='combine-comms-procs'></ul>" 
          <* jsWriteTo "combine-comms-procs" "<li>1+1</li>"        
          <* jsWriteTo "combine-comms-procs" "<li>'Hello'</li>"        
          <* jsWriteTo "combine-comms-procs" "<li>'True'</li>"
          <* jsWriteTo "combine-comms-procs" "<li>.. and 5 commands ...</li>"
          
        assert e v5 (2,"Hello",True)
        scroll e "cursor"

        write e "<h3>Single Promise</h3>"

        v6 :: Result String <- JS.send e $
              fromJSON <$> JS.procedure "new Promise(function(good,bad) { good('Hello') })"


        assert e v6 "Hello"

        scroll e "cursor"

        write e "<h3>Promises</h3>"

        v7 :: Result (String,String) <- JS.send e $ liftA2 (,)
             <$> (fromJSON <$> JS.procedure "new Promise(function(good,bad) { good('Hello') })")
             <*> (fromJSON <$> JS.procedure "new Promise(function(good,bad) { good('World') })")

        assert e v7 ("Hello","World")

        scroll e "cursor"

        write e "<h3>Sending Combine Commands and Procedures and Promises</h3>"

        v8 :: Result (Int,String,Bool) <- JS.send e $ liftA3 (,,)
          <$> (fromJSON <$> JS.procedure "1 + 1")
          <*> (fromJSON <$> JS.procedure "new Promise(function(good,bad) { good('World') })")
          <*> (fromJSON <$> JS.procedure "true")
          <* jsWriteTo "cursor" "<ul id='combine-comms-procs'></ul>" 
          <* jsWriteTo "combine-comms-procs" "<li>1+1</li>"        
          <* jsWriteTo "combine-comms-procs" "<li>promise of 'World'</li>"        
          <* jsWriteTo "combine-comms-procs" "<li>'True'</li>"
          <* jsWriteTo "combine-comms-procs" "<li>.. and 5 commands ...</li>"

        assert e v8 (2,"World",True)

