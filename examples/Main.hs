{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}


module Main where

import Web.Scotty
import Network.JavaScript
  
import Paths_javascript_bridge

main :: IO ()
main = main_ 3000

main_ :: Int -> IO ()
main_ i = do
  dataDir <- getDataDir
  -- dataDir <- return "." -- use for debugging
  scotty i $ do
    middleware $ start app  
    get "/" $ file $ dataDir ++ "/examples/Main.html"


app :: Engine -> IO ()
app eng = do
  send eng $ do
    command $ call "console.log" [string "starting..."]
    render "Hello!"

-- It is good practice to reflect the JavaScript utilties
-- you are using as typed Haskell functions.
render :: Command f => String -> f ()
render t = command $ call "jsb.render" [value t]
    
 
