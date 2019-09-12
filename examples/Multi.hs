{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Network.JavaScript
import Data.Semigroup
import Text.Read(readMaybe)
  
import Paths_javascript_bridge

main :: IO ()
main = main_ 3000

data Applets
  = Commands
  | Procedures
  | Constructors
  | Promises
  deriving (Eq, Ord, Show, Read, Enum, Bounded)


main_ :: Int -> IO ()
main_ i = do
  dataDir <- getDataDir
  --dataDir <- return "." -- use for debugging
  scotty i $ do
    middleware $ start app
    -- Any path, including /, returns the contents of Main.hs
    get "/:cmd" $ file $ dataDir ++ "/examples/Main.html"

app :: Engine -> IO ()
app eng = do
  -- simple dispatch based on location
  hash <- send eng $ procedure "window.location.pathname"
  case hash of
    "/" -> indexPage eng
    '/':xs -> case readMaybe xs of
                Just sub -> applet eng sub
                Nothing -> return ()
    _ -> return ()

indexPage :: Engine -> IO ()
indexPage eng = send eng $ render $
      "<ol>" <> Prelude.concat
        ["<li><a href=\"/" <> show a <> "\">"
          <> show a
          <> "</a></li>"
        | a <- [minBound .. maxBound] :: [Applets]
        ] <>
      "</ol>"

applet :: Engine -> Applets -> IO ()
applet eng Commands = do
  send eng $ do
    render "Hello World!"
applet eng Procedures = do
  send eng $ do
    -- The addition is done using JavaScript
    n <- addition 1.0 2.0
    render $ "1.0 + 2.0 = " ++ show n
applet eng Constructors = do
  send eng $ do
    -- Here, we construct an object, and it remotely
    o <- constructor "{n : 1}"
    n :: Int <- procedure $ var o <> ".n"
    render $ show n
    command $ var o <> ".n++"
    n :: Int <- procedure $ var o <> ".n"
    render $ show n
applet eng Promises = do
  send eng $ do
    -- When a promise is returned by a procedure,
    -- then we wait for it to resolve.
    p1 <- procedure $
      "new Promise((resolve,reject) => {" <>
      "  setTimeout(() => { resolve('Done p1')}, 1000)"  <>
      "})"
    render $ show (p1 :: String)
  send eng $ do
    -- When we group return values, we wait for all
    -- promises to resolve before returning to Haskell.
    -- Internally, this uses Promises.all()
    (p2,p3) <- (,) <$>
       (procedure $
         "new Promise((resolve,reject) => {" <>
          "  setTimeout(() => { resolve('Done p2')}, 1000)"  <>
          "})") <*>
       (procedure $
         "new Promise((resolve,reject) => {" <>
          "  setTimeout(() => { resolve('Done p3')}, 500)"  <>
          "})")
    render $ show (p2 :: String, p3 :: String)
  send eng $ do
    -- constructors create remote handles, and the
    -- handle can be a promise, and is returned
    -- immeduately.
    p2 <- constructor $
      "new Promise((resolve,reject) => {" <>
      "  setTimeout(() => { resolve('Done p4')}, 1000)"  <>
      "})"
    command $ var p2 <> ".then((v) => { jsb.render(JSON.stringify(v)) })"

    
-- It is good practice to reflect the JavaScript utilties
-- you are using as typed Haskell functions.
render :: Command f => String -> f ()
render t = command $ call "jsb.render" [value t]

addition :: Procedure f => Double -> Double -> f Double
addition a b = procedure $ value a <> "+" <> value b
