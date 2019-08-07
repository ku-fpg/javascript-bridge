{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}


module Main where

-- import Data.Aeson             
import Web.Scotty

import Network.JavaScript.ElmArchitecture

import Paths_javascript_bridge

-- The Up/Down example of the elm architecture
newtype Counter = Counter Int
  deriving (Eq, Ord, Enum, Num, Show)

instance Widget Counter where
  widget (Counter n) = object 
        [ "down" := wait $ Counter (n-1)
        , "text" := send (show n)
        , "up"   := wait $ Counter (n+1)
        ]

main :: IO ()
main = main_ 3000

main_ :: Int -> IO ()
main_ i = do
  dataDir <- getDataDir
--  dataDir <- return "."
  scotty i $ do
    get "/" $ file $ dataDir ++ "/examples/UpDown.html"
    middleware $ elmArchitecture $ Counter 0
            
