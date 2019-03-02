{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.JavaScript.Widgets where

import Network.JavaScript.ElmArchitecture

newtype Slider = Slider Double
  deriving (Eq, Ord, Show)

-- This is an example of the type being used for both
-- the message and the state.
instance Widget Slider Slider where
  update m _ = m
  view (Slider n) = object 
      [ "value"  := send n
      , "slider" := Slider <$> recv
      ]
