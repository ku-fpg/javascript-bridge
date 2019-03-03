{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty

import Network.JavaScript.ElmArchitecture
import Network.JavaScript.Widgets(Slider(..))

import Paths_javascript_bridge

main :: IO ()
main = main_ 3000

main_ :: Int -> IO ()
main_ i = do
  dataDir <- getDataDir
--  dataDir <- return "."
  scotty i $ do
    get "/" $ file $ dataDir ++ "/examples/Sliders.html"
    middleware $ elmArchitecture $
      map Slider [0,10..50]
