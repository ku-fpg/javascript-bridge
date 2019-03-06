{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Network.JavaScript.Widgets where

import Network.JavaScript.ElmArchitecture

newtype Slider = Slider Double
  deriving (Eq, Ord, Show)

instance Widget Double Double where
  widget n = object 
      [ "value"  := send n
      , "event"  := recv
      ]

-- deriving via?
instance Widget Slider Slider where
  widget (Slider n) = object 
      [ "value"  := send n
      , "event"  := Slider <$> recv
      ]
