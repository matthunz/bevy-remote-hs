{-# LANGUAGE OverloadedStrings #-}

module Data.Bevy.Remote (Request (..), RequestKind) where

import Data.Aeson

data RequestKind = ListRequest

data Request = Request Int RequestKind

instance ToJSON Request where
  toJSON (Request i ListRequest) =
    object
      [ "id" .= i,
        "method" .= ("bevy/list" :: String)
      ]
