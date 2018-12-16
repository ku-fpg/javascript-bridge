{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad (forever, foldM)
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
            html $ mconcat $
               [ "<head>"
                 -- From https://www.w3.org/Style/Examples/007/leaders.en.html
               , "<style type=\"text/css\">"
               , "ul.leaders {"
               , "max-width: 40em;"
               , "padding: 0;"
               , "overflow-x: hidden;"
               , "list-style: none}"
               , "ul.leaders li:before {"
               , "float: left;"
               , "width: 0;"
               , "white-space: nowrap;"
               , "content:"
               , "\". . . . . . . . . . . . . . . . . . . . \""
               , "\". . . . . . . . . . . . . . . . . . . . \""
               , "\". . . . . . . . . . . . . . . . . . . . \""
               , "\". . . . . . . . . . . . . . . . . . . . \"}"
               , "ul.leaders span:first-child {"
               , "padding-right: 0.33em;"
               , "background: white}"
               , "ul.leaders span + span {"
               , "float: right;"
               , "padding-left: 0.33em;"
               , "background: white}"
               , "span.pass{color: green;}"
               , "span.fail{color: red;}"
               , "</style>"
               , "</head>"
               , "<body>"
               ,   "<h1>JavaScript Bridge Tests</h1>"
               , T.pack (table tests) 
               ,   "<script>"
               ,     "jsb = new WebSocket('ws://' + location.host + '/');"
               ,     "jsb.onmessage = (evt) => eval(evt.data);"
                    -- remote object to allow interesting commands and procedures
               ,     "var remote = [];"
               ,   "</script>"
               , "</body>"
               ]

        takeMVar lock

data Test
 = Group String [Test]
 | TestA String (forall f . (RemoteProcedure f, Applicative f) => API f -> IO (Maybe String))
 | TestM String (forall f . (RemoteProcedure f, Monad f)       => API f -> IO (Maybe String))

data API f = API
 { send :: forall a . f a -> IO a
 , recv :: IO (Result Value)
 }

------------------------------------------------------------------------------

tests :: [Test]
tests =
  [ Group "Commands"
    [ TestA "command" $ \ API{..} -> send (command "1") >> pure Nothing
    ]
  , Group "Procedures"
    [ TestA "procedure 1 + 1" $ \ API{..} -> do
        v :: Result Int <- fromJSON <$> send (procedure "1+1")
        assert v (2 :: Int)
    , TestA "procedure 'Hello'" $ \ API{..} -> do
        v :: Result String <- fromJSON <$> send (procedure "'Hello'")
        assert v ("Hello" :: String)
    , TestA "procedure [true,false]" $ \ API{..} -> do
        v :: Result [Bool] <- fromJSON <$> send (procedure "[true,false]")
        assert v [True,False]
    ]
  , Group "Combine Commands / Procedure"
    [ TestA "command [] + push" $ \ API{..} -> do
        send (command "local = []" *> command "local.push(99)")        
        v :: Result [Int] <- fromJSON <$> send (procedure "local")
        assert v [99]
    , TestA "command [] + push + procedure" $ \ API{..} -> do
        v :: Result [Int] <- fromJSON <$> send (command "local = []" *> command "local.push(99)" *> procedure "local")
        assert v [99]
    , TestA "procedure + procedure" $ \ API{..} -> do
        v :: Result (Int,Bool) <- send (liftA2 (,)
                                            <$> (fromJSON <$> procedure "99")
                                            <*> (fromJSON <$> procedure "false"))
        assert v (99,False)
    ]    
  , Group "Promises"
    [ TestA "promises" $ \ API{..} -> do
        v :: Result (String,String) <- send $ liftA2 (,)
             <$> (fromJSON <$> procedure "new Promise(function(good,bad) { good('Hello') })")
             <*> (fromJSON <$> procedure "new Promise(function(good,bad) { good('World') })")
        assert v ("Hello","World")
    , TestA "promise + procedure" $ \ API{..} -> do
        v :: Result (String,String) <- send $ liftA2 (,)
             <$> (fromJSON <$> procedure "new Promise(function(good,bad) { good('Hello') })")
             <*> (fromJSON <$> procedure "'World'")
        assert v ("Hello","World")
    , TestA "good and bad promises" $ \ API{..} -> do
        v :: Either JavaScriptException (Result (String,String,String)) <- E.try $ send $ liftA3 (,,)
             <$> (fromJSON <$> procedure "new Promise(function(good,bad) { good('Hello') })")
             <*> (fromJSON <$> procedure "new Promise(function(good,bad) { bad('Promise Reject') })")
             <*> (fromJSON <$> procedure "new Promise(function(good,bad) { good('News') })")
        assert (pure v) (Left $ JavaScriptException $ String "Promise Reject")        
    ]
  , Group "Constructors"
    [ TestA "constructor" $ \ API{..} -> do
        rv :: RemoteValue <- send $ constructor "'Hello'"
        v1 :: Result String <- send $ fromJSON <$> procedure (var rv)
        send $ delete rv        
        v2 :: Value <- send $ procedure (var rv)
        assert ((,) <$> v1 <*> pure v2) ("Hello",Null)
    ]
  , Group "Exceptions"
    [ TestA "command throw" $ \ API{..} -> do
        send $ command $ "throw 'Command Fail';"
        assert (pure ()) ()
    , TestA "procedure throw" $ \ API{..} -> do
        v :: Either JavaScriptException Value <- E.try $ send $ procedure $ "(function(){throw 'Command Fail';})()"
        assert (pure v) (Left $ JavaScriptException $ String "Command Fail")
    ]
  , Group "Functions"
    [ TestA "function $ id" $ \ API{..} -> do
        rv :: RemoteValue <- send $ function $ \ v -> pure v
        v :: Result Int <- send $ fromJSON <$> procedure (val rv <> "(4)");
        assert v (4 :: Int)
    ]
  , Group "Events"
    [ TestA "event" $ \ API{..} -> do
        send $ command ("event('Hello, World')");        
        event <- recv 
        assert event (String "Hello, World" :: Value)
    ]
  , Group "Remote Monad"
    [ TestM "remote monad procedure chain" $ \ API{..} -> do
        vs :: Result Int <- fromJSON <$>
          (send $ foldM (\ (r :: Value) (i :: Int) -> procedure $ val r <> "+" <> val i)
                           (toJSON (0 :: Int))
                           [0..100])
        assert vs (sum [0..100])
    , TestM "remote monad constructor chain" $ \ API{..} -> do
        rv <- send $ constructor "0"
        rv :: RemoteValue <- 
          (send $ foldM (\ (r :: RemoteValue) (i :: Int) -> constructor $ val r <> "+" <> val i)
                           rv
                           [0..100])
        v :: Result Int <- fromJSON <$> (send $ procedure $ val rv)
        assert v (sum [0..100])
    ]
  , Group "Alive Connection"
    [ TestM "before wait" $ \ API{..} -> do
        assert (pure ()) ()
    , TestM "after wait for 80" $ \ API{..} -> do
        send $ command ("event('Hello, World')");        
        let w = 80
        _ <- threadDelay $ w * 1000 * 1000
        assert (pure ()) ()
    ]
  ]

------------------------------------------------------------------------------

assert :: (Eq a, Show a) => Result a -> a -> IO (Maybe String)
assert (Error er)  _ = return $ Just $ show er
assert (Success n) g
  | n == g    = return $ Nothing
  | otherwise = return $ Just $ show ("assert failure",n,g)

table :: [Test] -> String
table ts = go0 [] ts 
  where
    go0 p ts = "<ul>" ++ concatMap (\ (t,n) -> go (n:p) t) (zip ts [0..]) ++ "</ul>"

    go p (Group txt ts) = "<li>" ++ txt ++ "</li>" ++ go0 p ts
    go p (TestA txt _) = "<li><span>" ++ txt ++ "</span>...." ++
                       "<span><span id=\"" ++ tag p ++ "-m\">Monad</span>, " ++
                       "<span id=\"" ++ tag p ++ "-a\">Applicative</span></span>"
    go p (TestM txt _) = "<li><span>" ++ txt ++ "</span>...." ++
                       "<span id=\"" ++ tag p ++ "-m\">Monad</span>"

tag :: [Int] -> String
tag p = "tag" ++ concatMap (\ a -> '-' : show a) p

runTest :: Engine -> [Int] -> Test -> IO ()
runTest e p (Group txt ts) = runTests e p ts
runTest e p (TestA txt k) = do
  recv <- doRecv e
  doTest (API (JS.send e) recv)  "-m" p k
  doTest (API (JS.sendA e) recv) "-a" p k
runTest e p (TestM txt k) = do
  recv <- doRecv e
  doTest (API (JS.send e) recv)  "-m" p k

doRecv :: Engine -> IO (IO (Result Value))
doRecv e = do
  es <- newTChanIO
  setListener e $ atomically . writeTChan es
  return $ do
        wait <- registerDelay $ 1000 * 1000
        atomically $ (pure <$> readTChan es)
                     `orElse` (do b <- readTVar wait ; check b ; return $ Error "timeout!")        
doTest :: Remote f => API f -> String -> [Int] -> (API f -> IO (Maybe String)) -> IO ()
doTest api@API{..} suff p k = do
  rM <- k api
  case rM of
    Nothing -> send $ command $ "document.getElementById('" <> T.pack (tag p ++ suff) <> "').classList.add('pass');"
    Just msg -> do
      print ("doTest failed"::String,msg)
      send $ command $ "document.getElementById('" <> T.pack (tag p ++ suff) <> "').classList.add('fail');"

runTests :: Engine -> [Int] -> [Test] -> IO ()
runTests e p ts = sequence_ [ runTest e (n:p) t | (t,n) <- ts `zip` [0..]]

example :: Engine -> IO ()
example e = runTests e [] tests
