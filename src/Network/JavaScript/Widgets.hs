{-# LANGUAGE MultiParamTypeClasses #-}

module Network.JavaScript.Widgets where

import Network.JavaScript.ElmArchitecture

newtype Slider = Slider Double
  deriving (Eq, Ord, Show)

instance Widget Slider Slider where
  widget (Slider n) = Slider <$> widget n

