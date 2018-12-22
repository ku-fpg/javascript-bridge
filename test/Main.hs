{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad (forever, foldM)
import Data.Aeson
import Data.Monoid((<>))
import qualified Data.Text.Lazy as T
import Network.JavaScript as JS
import Network.Wai.Middleware.RequestLogger
import System.Exit
import Web.Scotty hiding (delete, function)

main = main_ 3000

main_ :: Int -> IO ()
main_ i = do
        lock <- newEmptyMVar

        _ <- forkIO $ scotty i $ do
--          middleware $ logStdout
          
          middleware $ start $ \ e -> example e `E.finally`
                       (do putMVar lock ()
                           putStrLn "Finished example")


          get "/" $ do
            html $ mconcat $
               [
                 "<!doctype html>"
               , "<html lang=\"en\">"
               , "<head>"
               , "<!-- Required meta tags -->"
               , "<meta charset=\"utf-8\">"
               , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1, shrink-to-fit=no\">"
               , "<!-- Bootstrap CSS -->"
               , "<link rel=\"stylesheet\" href=\"https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css\" integrity=\"sha384-MCw98/SFnGE8fJT3GXwEOngsV7Zt27NXFoaoApmYm81iuXoPkFOJwJ8ERdknLPMO\" crossorigin=\"anonymous\">"

               , "<title>JavaScript Bridge Tests</title>"
               , "</head>"
               , "<body>"
               , " <div class=\"container\">"
               ,   "<h3>JavaScript Bridge Tests</h3>"
               , "  <div class=\"row\">"
               , "    <div class=\"col-3\"><p class=\"font-weight-bold\">Groups</p></div>"
               , "    <div class=\"col-5\"><p class=\"font-weight-bold\">Tests</p></div>"
               , "    <div class=\"col-2\"><p class=\"text-center font-weight-bold\">Applicative</p></div>"
               , "    <div class=\"col-2\"><p class=\"text-center font-weight-bold\">Monad</p></div>"
               , "  </div>"
               , T.pack (table tests)
               , "</div>"
               ,   "<script>"
               ,     "jsb = new WebSocket('ws://' + location.host + '/');"
               ,     "jsb.onmessage = (evt) => eval(evt.data);"
                    -- remote object to allow interesting commands and procedures
               ,     "var remote = [];"
               ,   "</script>"
               , "</body>"
               , "</html>"
               ]

        takeMVar lock

data Test
 = TestA String (forall f . (Command f, Procedure f, Applicative f) => API f -> IO (Maybe String))
 | TestM String (forall f . (Command f, Procedure f, Monad f)       => API f -> IO (Maybe String))

data Tests = Tests String [Test]

data API f = API
 { send :: forall a . f a -> IO a
 , recv :: IO (Result Value)
 }

------------------------------------------------------------------------------

tests :: [Tests]
tests =
  [ Tests "Commands"
    [ TestA "command" $ \ API{..} -> send (command "1") >> pure Nothing
    ]
  , Tests "Procedures"
    [ TestA "procedure 1 + 1" $ \ API{..} -> do
        v :: Int <- send (procedure "1+1")
        assert (pure v) (2 :: Int)
    , TestA "procedure 'Hello'" $ \ API{..} -> do
        v :: String <- send (procedure "'Hello'")
        assert (pure v) ("Hello" :: String)
    , TestA "procedure [true,false]" $ \ API{..} -> do
        v :: [Bool] <- send (procedure "[true,false]")
        assert (pure v) [True,False]
    ]
  , Tests "Combine Commands / Procedure"
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
  , Tests "Promises"
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
  , Tests "Constructors"
    [ TestA "constructor" $ \ API{..} -> do
        rv :: RemoteValue () <- send $ constructor "'Hello'"
        v1 :: Result String <- send $ fromJSON <$> procedure (var rv)
        send $ delete rv        
        v2 :: Value <- send $ procedure (var rv)
        assert ((,) <$> v1 <*> pure v2) ("Hello",Null)
    ]
  , Tests "Exceptions"
    [ TestA "command throw" $ \ API{..} -> do
        send $ command $ "throw 'Command Fail';"
        assert (pure ()) ()
    , TestA "procedure throw" $ \ API{..} -> do
        v :: Either JavaScriptException Value <- E.try $ send $ procedure $ "(function(){throw 'Command Fail';})()"
        assert (pure v) (Left $ JavaScriptException $ String "Command Fail")
    ]
  , Tests "Functions"
    [ TestA "function $ id" $ \ API{..} -> do
        rv :: RemoteValue (Int -> IO Int) <- send $ function $ \ v -> pure v
        v :: Result Int <- send $ fromJSON <$> procedure (val rv <> "(4)");
        assert v (4 :: Int)
    ]
  , Tests "Events"
    [ TestA "event" $ \ API{..} -> do
        send $ command ("event('Hello, World')");        
        event <- recv 
        assert event (String "Hello, World" :: Value)
    ]
  , Tests "Remote Monad"
    [ TestM "remote monad procedure chain" $ \ API{..} -> do
        vs :: Result Int <- fromJSON <$>
          (send $ foldM (\ (r :: Value) (i :: Int) -> procedure $ val r <> "+" <> val i)
                           (toJSON (0 :: Int))
                           [0..100])
        assert vs (sum [0..100])
    , TestM "remote monad constructor chain" $ \ API{..} -> do
        rv <- send $ constructor "0"
        rv :: RemoteValue () <- 
          (send $ foldM (\ (r :: RemoteValue ()) (i :: Int) -> constructor $ val r <> "+" <> val i)
                           rv
                           [0..100])
        v :: Result Int <- fromJSON <$> (send $ procedure $ val rv)
        assert v (sum [0..100])
    ]
  , Tests "Alive Connection"
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

table :: [Tests] -> String
table ts = go0 [] ts 
  where
    go0 p ts = concatMap (\ (t,n) -> go1 (n:p) t) (zip ts [0..])

    go1 p (Tests txt ts) = unlines
      [ "<div class=\"row\">" ++
        "<div class=\"col-3\">" ++
         pre ++
        "</div>" ++
        "<div class=\"col-5\">" ++
        tst ++
        "</div>" ++
        "<div class=\"col-2\">" ++
        mon ++
        "</div>" ++
        "<div class=\"col-2\">" ++
        app ++
        "</div>" ++        
        "</div>"
      | (t,n) <- ts `zip` [0..]
      , let pre | n == 0 = txt
                | otherwise = ""
      , let (tst,mon,app) = go (n:p) t
      ]

    go p (TestA txt _) = (txt,bar p "a",bar p "m")
    go p (TestM txt _) = (txt,"",bar p "m")

    bar p a = "<div class=\"progress\"><div id=\"" ++ tag p ++ "-" ++ a ++ "\" class=\"progress-bar\" role=\"progressbar\" style=\"width: 0%;\"></div></div>"

tag :: [Int] -> String
tag p = "tag" ++ concatMap (\ a -> '-' : show a) p

runTest :: Engine -> [Int] -> Test -> IO ()
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

doTest :: (Applicative f, Command f) => API f -> String -> [Int] -> (API f -> IO (Maybe String)) -> IO ()
doTest api@API{..} suff p k = do
  print ("do Test" :: String)
  rM <- k api
  case rM of
    Nothing -> do
      send $
       (command $ "document.getElementById('" <> T.pack (tag p ++ suff) <> "').style.width='100%'") *>
       (command $ "document.getElementById('" <> T.pack (tag p ++ suff) <> "').classList.add('bg-success')")
    Just msg -> do
      print ("doTest failed"::String,msg)
      send $
       (command $ "document.getElementById('" <> T.pack (tag p ++ suff) <> "').style.width='100%'") *>
       (command $ "document.getElementById('" <> T.pack (tag p ++ suff) <> "').classList.add('bg-danger')")

runTests :: Engine -> [Int] -> [Tests] -> IO ()
runTests e p ts = sequence_ [ runTest e (m:n:p) t | (Tests _ ts,n) <- ts `zip` [0..], (t,m) <- ts `zip` [0..] ]

example :: Engine -> IO ()
example e = runTests e [] tests

{-
-- Some GADT magic to generate typed permutations of monadic actions.
data X where
  X :: LocalMonad a -> RemoteMonad a -> (a -> Value) -> X 

data LocalMonad a where
  Return :: LocalMonad ()                 -- pure
  Push   :: Int -> LocalMonad ()          -- command
  Get    :: LocalMonad Int                -- procedure
  
xTests =
  [ X Return (return ()) (\ () -> True)
  ]

[X] -> Local
-}
