{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty

import Network.JavaScript.ElmArchitecture

import Paths_javascript_bridge

-- The Slider example of the elm architecture
newtype Slider = Slider Double
  deriving (Eq, Ord, Show)

-- This is an example of the slider being used for both
-- the message and the state.
instance Widget Slider Slider where
  update m _ = m
  view (Slider n) = object 
      [ "text"    := send n
      , "slider"  := Slider <$> recv
      ]

main :: IO ()
main = main_ 3000

main_ :: Int -> IO ()
main_ i = do
  dataDir <- getDataDir
--  dataDir <- return "."
  scotty i $ do
    get "/" $ file $ dataDir ++ "/examples/Slider.html"
    middleware $ elmArchitecture $ Slider 20
            
