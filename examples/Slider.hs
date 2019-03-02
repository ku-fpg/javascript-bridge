{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty

import Network.JavaScript.ElmArchitecture

import Paths_javascript_bridge

-- The Slider example of the elm architecture

data Msg = SlideTo Double
  deriving (Eq, Ord, Show)

example :: ElmArchitecture () Msg Double
example = ElmArchitecture{..}
  where
    update (SlideTo v) _ = pure v
    view   n    = object
      [ "text"    := send n
      , "slider"  := SlideTo <$> recv
      ] 
    runtime _ = error "no runtime needed"

main :: IO ()
main = main_ 3000

main_ :: Int -> IO ()
main_ i = do
  dataDir <- getDataDir
--  dataDir <- return "."
  scotty i $ do
    get "/" $ file $ dataDir ++ "/examples/Slider.html"
    middleware $ elmArchitecture example 20
            
