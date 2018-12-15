{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}


import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad (forever)
import Data.Aeson
import Data.Monoid((<>))
import qualified Data.Text.Lazy as T
import Network.JavaScript as JS
import System.Exit
import Web.Scotty hiding (delete, function)

main :: IO ()
main = do
        lock <- newEmptyMVar

        _ <- forkIO $ scotty 3000 $ do
          middleware $ start $ \ e -> example e `E.finally`
                       (do putMVar lock ()
                           putStrLn "Finished example")


          get "/" $ do
            html $ mconcat
               [ "<body>"
               ,   "<h1>JavaScript Bridge Tests</h1>"
               ,    "<h3>Sending Commands</h3>"
               ,    "<ul><li id='send-command'><i style='color: #ff0000'>waiting for send $ command</i></li></ul>"
               ,   "<div id='cursor'>"
                 -- Include this code in your page
               ,   "<script>"
               ,     "jsb = new WebSocket('ws://' + location.host + '/');"
               ,     "jsb.onmessage = (evt) => eval(evt.data);"               
               ,   "</script>"
               , "</body>"
               ]

        takeMVar lock


write :: Engine -> String -> IO ()
write e txt = send e $ command ("document.getElementById('cursor').innerHTML += " <> T.pack (show txt))

jsWriteTo :: String -> String -> Packet ()
jsWriteTo i txt = command ("document.getElementById('" <> T.pack i <> "').innerHTML += " <> T.pack (show txt))

writeTo:: Engine -> String -> String -> IO ()
writeTo e i txt = send e $ command ("document.getElementById('" <> T.pack i <> "').innerHTML = " <> T.pack (show txt))

scroll :: Engine -> String -> IO ()
scroll e i = send e $ command $ "document.getElementById('" <> T.pack i <> "').scrollIntoView({behavior: 'smooth', block: 'end'})"


assert :: (Eq a, Show a) => Engine -> Result a -> a -> IO ()
assert e (Error er) _ = do
  write e $ "<ul><li><span style='color: red'>Failed to parse result: " ++ show er ++ "</span></ul></li>"
  exitFailure
assert e (Success n) g
   | n == g = do
       write e $ "<ul><li>result parsed as " ++ show n ++ ", and is correct</li></ul>"
       scroll e "cursor"
   | otherwise = do
       write e $ "<ul><li><span style='color: red'>unexpected result : '" ++ show n ++ "'</span></li></ul>"
       exitFailure

example :: Engine -> IO ()
example e = do
        es <- newTChanIO
        addListener e $ atomically . writeTChan es

        pid <- forkIO $ forever $ do
          ev <- atomically $ readTChan es
          print ("Unexpected event"::String,ev::Value)

        -- First test of send command
        writeTo e "send-command" "send $ command ... works"
        scroll e "cursor"

        -- We assume send command works; and test procedures
        write e "<h3>Sending Procedures</h3>"
        v1 :: Value <- send e (procedure "1 + 1")
        write e "<ul><li>send $ procedure (1+1) works</li></ul>"

        assert e (fromJSON v1) (2 :: Int)

        v2 :: Value <- send e (procedure "'Hello'")
        write e "<ul><li>send $ procedure 'Hello' works</li></ul>"

        assert e (fromJSON v2) ("Hello" :: String)

        v3 :: Value <- send e (procedure "[true,false]")
        write e "<ul><li>send $ procedure [true,false] works</li></ul>"

        assert e (fromJSON v3) [True,False]

        write e "<h3>Sending Combine Commands</h3>"
        send e $ 
             jsWriteTo "cursor" "<ul id='combine-commands'></ul>" 
          <* jsWriteTo "combine-commands" "<li>command 1</li>"        
          <* jsWriteTo "combine-commands" "<li>command 2</li>"        

        scroll e "cursor"

        -- TODO: add test of returning a object with values, and an array.
        write e "<h3>Sending Combine Procedures</h3>"
        v4 :: Result (Int,String,Bool) <- send e $ liftA3 (,,)
          <$> (fromJSON <$> procedure "1 + 1")
          <*> (fromJSON <$> procedure "'Hello'")
          <*> (fromJSON <$> procedure "true")
        send e $ 
             jsWriteTo "cursor" "<ul id='combine-procs'></ul>" 
          <* jsWriteTo "combine-procs" "<li>1+1</li>"        
          <* jsWriteTo "combine-procs" "<li>'Hello'</li>"        
          <* jsWriteTo "combine-procs" "<li>'True'</li>"
  
        assert e v4 (2,"Hello",True)

        write e "<h3>Sending Combine Commands and Procedures</h3>"

        v5 :: Result (Int,String,Bool) <- send e $ liftA3 (,,)
          <$> (fromJSON <$> procedure "1 + 1")
          <*> (fromJSON <$> procedure "'Hello'")
          <*> (fromJSON <$> procedure "true")
          <* jsWriteTo "cursor" "<ul id='combine-comms-procs'></ul>" 
          <* jsWriteTo "combine-comms-procs" "<li>1+1</li>"        
          <* jsWriteTo "combine-comms-procs" "<li>'Hello'</li>"        
          <* jsWriteTo "combine-comms-procs" "<li>'True'</li>"
          <* jsWriteTo "combine-comms-procs" "<li>.. and 5 commands ...</li>"
          
        assert e v5 (2,"Hello",True)

        write e "<h3>Single Promise</h3>"

        v6 :: Result String <- send e $
              fromJSON <$> procedure "new Promise(function(good,bad) { good('Hello') })"


        assert e v6 "Hello"

        write e "<h3>Promises</h3>"

        v7 :: Result (String,String) <- send e $ liftA2 (,)
             <$> (fromJSON <$> procedure "new Promise(function(good,bad) { good('Hello') })")
             <*> (fromJSON <$> procedure "new Promise(function(good,bad) { good('World') })")

        assert e v7 ("Hello","World")

        write e "<h3>Sending Combine Commands and Procedures and Promises</h3>"

        v8 :: Result (Int,String,Bool) <- send e $ liftA3 (,,)
          <$> (fromJSON <$> procedure "1 + 1")
          <*> (fromJSON <$> procedure "new Promise(function(good,bad) { good('World') })")
          <*> (fromJSON <$> procedure "true")
          <* jsWriteTo "cursor" "<ul id='combine-comms-procs'></ul>" 
          <* jsWriteTo "combine-comms-procs" "<li>1+1</li>"        
          <* jsWriteTo "combine-comms-procs" "<li>promise of 'World'</li>"        
          <* jsWriteTo "combine-comms-procs" "<li>'True'</li>"
          <* jsWriteTo "combine-comms-procs" "<li>.. and 5 commands ...</li>"

        assert e v8 (2,"World",True)

        write e "<h3>Builder</h3>"

        rv :: RemoteValue <- send e $
              constructor "\"Hello\""
        write e "<ul><li>send $ constructor \"Hello\" works</li></ul>"
        
        -- hard wired to 40, from observation
        assert e (return (show rv)) "RemoteValue 23"

        -- force reading of this remote value (not possible in general)
        v6 :: Result String <- send e $
              fromJSON <$> procedure (var rv)
        write e "<ul><li>send $ procedure (var rv)</li></ul>"

        send e $ delete rv
        write e "<ul><li>send $ delete rv</li></ul>"

        -- read after delete; should return Null
        v6 :: Result Value <- send e $
              fromJSON <$> procedure (var rv)
        write e "<ul><li>send $ procedure (var rv) (again)</li></ul>"

        assert e v6 Null

        write e "<h3>Exceptions</h3>"
        send e $ command $ "throw 'Command Fail';"
        write e "<ul><li>send $ command (throw ..) sent (result ignored)</li></ul>"
        v9 :: Either JavaScriptException Value <- E.try $ send e $ procedure $ "(function(){throw 'Procedure Fail'})();"
        scroll e "cursor"
        write e "<ul><li>send $ procedure (throw ..) sent</li></ul>"
        case v9 of
          Right _ -> do
            write e "<ul><li style='color: red'>send $ procedure (throw ..) replied with result </li></ul>"
            exitFailure
          Left (JavaScriptException v) -> assert e (fromJSON v) ("Procedure Fail" :: String)

        v10 :: Either JavaScriptException (Result (String,String,String)) <- E.try $ send e $ liftA3 (,,)
             <$> (fromJSON <$> procedure "new Promise(function(good,bad) { good('Hello') })")
             <*> (fromJSON <$> procedure "new Promise(function(good,bad) { bad('Promise Reject') })")
             <*> (fromJSON <$> procedure "new Promise(function(good,bad) { good('News') })")

        write e "<ul><li>send $ procedure (3 promises, 1 rejected) sent</li></ul>"
        case v10 of
          Right _ -> do
            write e "<ul><li style='color: red'>send $ procedure (throw ..) replied with result </li></ul>"
            exitFailure
          Left (JavaScriptException v) -> assert e (fromJSON v) ("Promise Reject" :: String)

        v11 :: Either Value (Result (String,String,String)) <- sendE e $ liftA3 (,,)
             <$> (fromJSON <$> procedure "new Promise(function(good,bad) { good('Hello') })")
             <*> (fromJSON <$> procedure "new Promise(function(good,bad) { bad('Promise Reject') })")
             <*> (fromJSON <$> procedure "new Promise(function(good,bad) { good('News') })")

        write e "<ul><li>sendE $ procedure (3 promises, 1 rejected) sent</li></ul>"
        case v11 of
          Right _ -> do
            write e "<ul><li style='color: red'>sendE $ procedure (throw ..) replied with result </li></ul>"
            exitFailure
          Left v -> assert e (fromJSON v) ("Promise Reject" :: String)


        write e "<h3>Higher Order Function</h3>"

        rv :: RemoteValue <- send e $ function $ \ v -> pure v
        write e "<ul><li>send $ function $ id works</li></ul>"

        assert e (return (show rv)) "RemoteValue 39"

        v :: Result Int <- send e $ fromJSON <$> procedure (val rv <> "(4)");
        write e "<ul><li>send $ procedure (rv(4))</li></ul>"

        assert e v (4 :: Int)

        write e "<h3>Events</h3>"

        killThread pid
        
        send e $ command ("event('Hello, World')");

        write e "<ul><li>send $ command $ event 'Hello, World'</li></ul>"
        wait <- registerDelay $ 1000 * 1000
        event :: Result String
              <- atomically $ (fromJSON <$> readTChan es)
                     `orElse` (do b <- readTVar wait ; check b ; return $ Error "timeout!")

        assert e event ("Hello, World" :: String)

        write e "<h3>Testing staying alive</h3>"

        let w = 80
        
        write e $ "<ul><li>waiting " ++ show w ++ " seconds...</li></ul>"
        scroll e "cursor"
        _ <- threadDelay $ w * 1000 * 1000
        write e $ "<ul><li>Waited. Connection still alive!</li></ul>"

        write e $ "<ul><li>Sending trivial procedure test</li></ul>"
        v1 :: Value <- send e (procedure "1 + 1")
        assert e (fromJSON v1) (2 :: Int)
        
        write e "<h2>All Tests Pass</h2>"
        scroll e "cursor"
