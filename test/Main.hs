{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad (forever, foldM, void)
import Data.Aeson
import Data.Monoid((<>))
import qualified Data.Text.Lazy as T
import Network.JavaScript as JS
import Network.Wai.Middleware.RequestLogger
import System.Exit
import Web.Scotty hiding (delete, function)
import Data.Time.Clock

main = main_ 3000

main_ :: Int -> IO ()
main_ i = do
        lock <- newEmptyMVar

        void $ forkIO $ scotty i $ do
--          middleware $ logStdout
          
          middleware $ start $ \ ev e -> example ev e `E.finally`
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
               ,     "window.jsb = {ws: new WebSocket('ws://' + location.host + '/')};"
               ,     "jsb.ws.onmessage = (evt) => eval(evt.data);"
                    -- remote object to allow interesting commands and procedures
               ,     "var remote = [];"
               ,     "var stepme = function(dom,s) {"
               ,     "   var start = null;"
               ,     "   function step(timestamp) {"
               ,     "      if (!start) start = timestamp;"
               ,     "      if (dom.classList.contains('bg-success') || dom.classList.contains('bg-danger')) return;"
               ,     "      var progress = parseInt((timestamp - start) * 100 / (1000 * s));"
               ,     "      dom.style.width='' + progress + '%';"
               ,     "      dom.innerHTML='' + progress + '%';"
               ,     "      if (progress < 100) {"
               ,     "        window.requestAnimationFrame(step);"
               ,     "      } else {"               
               ,     "       dom.classList.add('bg-danger');"
               ,     "      }"
               ,     "    }"
               ,     "    requestAnimationFrame(step);"
               ,     "};"
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
 , progressBar :: RemoteValue DOM
 }

data DOM

------------------------------------------------------------------------------

tests :: [Tests]
tests =
  [ Tests "Commands"
    [ TestA "command" $ \ API{..} -> send (command "1") >> pure Nothing
    ]
  , Tests "Procedures"
    [ TestA "procedure 1 + 1" $ \ API{..} -> do
        v :: Int <- send (procedure "1+1")
        assert v (2 :: Int)
    , TestA "procedure 'Hello'" $ \ API{..} -> do
        v :: String <- send (procedure "'Hello'")
        assert v ("Hello" :: String)
    , TestA "procedure [true,false]" $ \ API{..} -> do
        v :: [Bool] <- send (procedure "[true,false]")
        assert v [True,False]
    ]
  , Tests "Combine Commands / Procedure"
    [ TestA "command [] + push" $ \ API{..} -> do
        send (command "local = []" *> command "local.push(99)")        
        v :: [Int] <- send (procedure "local")
        assert v [99]
    , TestA "command [] + push + procedure" $ \ API{..} -> do
        v :: [Int] <- send (command "local = []" *> command "local.push(99)" *> procedure "local")
        assert v [99]
    , TestA "procedure + procedure" $ \ API{..} -> do
        v :: (Int,Bool) <- send (liftA2 (,)
                                            (procedure "99")
                                            (procedure "false"))
        assert v (99,False)
    ]    
  , Tests "Promises"
    [ TestA "promises" $ \ API{..} -> do
        v :: (String,String) <- send $ liftA2 (,)
                 (procedure "new Promise(function(good,bad) { good('Hello') })")
                 (procedure "new Promise(function(good,bad) { good('World') })")
        assert v ("Hello","World")
    , TestA "promise + procedure" $ \ API{..} -> do
        v :: (String,String) <- send $ liftA2 (,)
                 (procedure "new Promise(function(good,bad) { good('Hello') })")
                 (procedure "'World'")
        assert v ("Hello","World")
    , TestA "good and bad promises" $ \ API{..} -> do
        v :: Either JavaScriptException ((String,String,String)) <- E.try $ send $ liftA3 (,,)
                 (procedure "new Promise(function(good,bad) { good('Hello') })")
                 (procedure "new Promise(function(good,bad) { bad('Promise Reject') })")
                 (procedure "new Promise(function(good,bad) { good('News') })")
        assert v (Left $ JavaScriptException $ String "Promise Reject")        
    ]
  , Tests "Constructors"
    [ TestA "constructor" $ \ API{..} -> do
        rv :: RemoteValue () <- send $ constructor "'Hello'"
        v1 :: String <- send $ procedure (var rv)
        send $ delete rv        
        v2 :: Value <- send $ procedure (var rv)
        assert (v1,v2) ("Hello",Null)
    ]
  , Tests "Exceptions"
    [ TestA "command throw" $ \ API{..} -> do
        send $ command $ "throw 'Command Fail';"
        assert () ()
    , TestA "procedure throw" $ \ API{..} -> do
        v :: Either JavaScriptException Value <- E.try $ send $ procedure $ "(function(){throw 'Command Fail';})()"
        assert v (Left $ JavaScriptException $ String "Command Fail")
    ]
  , Tests "Functions"
    [ TestA "function $ id" $ \ API{..} -> do
        rv :: RemoteValue (Int -> IO Int) <- send $ function $ \ _ v -> pure v
        v :: Int <- send $ procedure (var rv <> "(4)");
        assert v (4 :: Int)
    ]
  , Tests "Events"
    [ TestA "event" $ \ API{..} -> do
        send $ command ("jsb.event('Hello, World')");        
        event <- recv
        assert event (Success $ toJSON ("Hello, World" :: String))
    ]
  , Tests "Remote Monad"
    [ TestM "remote monad procedure chain" $ \ API{..} -> do
        vs :: Value <- 
          (send $ foldM (\ (r :: Value) (i :: Int) -> procedure $ value r <> "+" <> value i)
                           (toJSON (0 :: Int))
                           [0..100])
        assert vs (toJSON $ sum [0..100::Int])
    , TestM "remote monad constructor chain" $ \ API{..} -> do
        rv <- send $ constructor "0"
        rv :: RemoteValue () <- 
          (send $ foldM (\ (r :: RemoteValue ()) (i :: Int) -> constructor $ value r <> "+" <> value i)
                           rv
                           [0..100])
        v :: Int <- (send $ procedure $ value rv)
        assert v (sum [0..100])
    ]
  , Tests "Alive Connection" $
    [ TestM "before wait" $ \ API{..} -> do
        assert () ()
    ] ++ [ TestM ("after wait for " ++ show w) $ \ API{..} -> do
        send $ command $ "stepme(" <> var progressBar <> "," <> value (fromIntegral w * 1.2 :: Float) <> ")"
        _ <- threadDelay $ w * 1000 * 1000
        assert () ()
        | w <- [3,10,80]
        ]
  ]

------------------------------------------------------------------------------

assert :: (Eq a, Show a) => a -> a -> IO (Maybe String)
assert n g
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

runTest :: Event Value -> Engine -> [Int] -> Test -> IO ()
runTest ev e p (TestA txt k) = do
  recv <- doRecv ev e
  mBar <- JS.send e $ constructor $ "document.getElementById('" <> T.pack (tag p ++ "-m") <> "')"
  aBar <- JS.send e $ constructor $ "document.getElementById('" <> T.pack (tag p ++ "-a") <> "')"
  doTest (API (JS.send e) recv mBar)  "-m" p k
  doTest (API (JS.sendA e) recv aBar) "-a" p k
runTest ev e p (TestM txt k) = do
  recv <- doRecv ev e
  mBar <- JS.send e $ constructor $ "document.getElementById('" <> T.pack (tag p ++ "-m") <> "')"
  doTest (API (JS.send e) recv mBar)  "-m" p k

doRecv :: Event Value -> Engine -> IO (IO (Result Value))
doRecv ev e = do
  es <- newTChanIO
  addListener ev $ atomically . writeTChan es
  return $ do
        wait <- registerDelay $ 1000 * 1000
        atomically $ (pure <$> readTChan es)
                     `orElse` (do b <- readTVar wait ; check b ; return $ Error "timeout!")        

doTest :: (Applicative f, Command f) => API f -> String -> [Int] -> (API f -> IO (Maybe String)) -> IO ()
doTest api@API{..} suff p k = do
  tm0 <- getCurrentTime
  rM <- k api
  tm1 <- getCurrentTime
  let tm = show (diffUTCTime tm1 tm0)
  case rM of
    Nothing -> do
      send $
       (command $ var progressBar <> ".style.width='100%'") *>
       (command $ var progressBar <> ".classList.add('bg-success')") *>
       (command $ var progressBar <> ".innerHTML=" <> value tm)       
    Just msg -> do
      print ("doTest failed"::String,msg)
      send $
       (command $ var progressBar <> ".style.width='100%'") *>
       (command $ var progressBar <> ".classList.add('bg-danger')") *>
       (command $ var progressBar <> ".innerHTML=" <> value tm)       
       
runTests :: Event Value -> Engine -> [Int] -> [Tests] -> IO ()
runTests ev e p ts = sequence_ [ runTest ev e (m:n:p) t | (Tests _ ts,n) <- ts `zip` [0..], (t,m) <- ts `zip` [0..] ]

example :: Event Value -> Engine -> IO ()
example ev e = runTests ev e  [] tests
