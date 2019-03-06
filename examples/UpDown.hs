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

-- Example up/down
data Msg = Up | Down
  deriving (Eq, Ord, Show)

newtype Counter = Counter Int
  deriving (Eq, Ord, Enum, Num, Show)

instance Widget Counter Counter where
  widget (Counter n) = object 
        [ "down" := Counter (n-1) <$ recv_
        , "text" := send (show n)
        , "up"   := Counter (n+1) <$ recv_
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
            
