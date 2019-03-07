{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}


module Main where

-- import Data.Aeson             
import Web.Scotty

import Network.JavaScript.ElmArchitecture
import Data.Text(Text)
  
import Paths_javascript_bridge

main :: IO ()
main = main_ 3000

main_ :: Int -> IO ()
main_ i = do
  dataDir <- getDataDir
  dataDir <- return "."
  scotty i $ do
    get "/" $ file $ dataDir ++ "/examples/Text.html"
    middleware $ elmArchitecture ("" :: Text)
            
