{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

-- import Data.Aeson             
import Web.Scotty

import Network.JavaScript.ElmArchitecture

import Paths_javascript_bridge

-- The Up/Down example of the elm architecture

-- Example up/down
data Msg = Up | Down
  deriving (Eq, Ord, Show)

example :: ElmArchitecture () Msg Int
example = ElmArchitecture{..}
  where
    update Up   = pure . succ
    update Down = pure . pred
    view   n    = object 
        [ "down" := Down <$ recv_
        , "text" := send (show n)
        , "up"   := Up <$ recv_
        ]
    runtime _ = error "no runtime needed"

main :: IO ()
main = main_ 3000

main_ :: Int -> IO ()
main_ i = do
  dataDir <- getDataDir
--  dataDir <- return "."
  scotty i $ do
    get "/" $ file $ dataDir ++ "/examples/UpDown.html"
    middleware $ elmArchitecture example 0
            
