{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Bevy.Remote.Transport
  ( Filter (..),
    newFilter,
    QueryData (..),
    RequestKind (..),
    Request (..),
    Response (..),
    SpawnResponse (..),
  )
where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM

data Filter = Filter
  { filterComponents :: [String],
    filterOptions :: [String],
    filterHas :: [String],
    filterWith :: [String],
    filterWithout :: [String]
  }
  deriving (Show)

instance Monoid Filter where
  mempty = newFilter

instance Semigroup Filter where
  Filter a b c d e <> Filter a' b' c' d' e' =
    Filter (a <> a') (b <> b') (c <> c') (d <> d') (e <> e')

newFilter :: Filter
newFilter = Filter [] [] [] [] []

data RequestKind
  = GetRequest Int [String] Bool
  | ListRequest
  | QueryRequest Filter
  | SpawnRequest (KM.KeyMap Value)
  deriving (Show)

instance ToJSON RequestKind where
  toJSON ListRequest = object []
  toJSON (GetRequest e ids strict) =
    object
      [ "entity" .= e,
        "components" .= ids,
        "strict" .= strict
      ]
  toJSON (QueryRequest f) =
    object
      [ "data"
          .= object
            [ "components" .= filterComponents f,
              "option" .= filterOptions f,
              "has" .= filterHas f
            ],
        "filter"
          .= object
            [ "with" .= filterWith f,
              "without" .= filterWithout f
            ]
      ]
  toJSON (SpawnRequest km) = object ["components" .= km]

data Request a = Request String Int (Maybe a) deriving (Show)

instance (ToJSON a) => ToJSON (Request a) where
  toJSON (Request method i params) =
    object
      ( [ "jsonrpc" .= ("2.0" :: String),
          "method" .= method,
          "id" .= i
        ]
          ++ case params of
            Just p -> ["params" .= p]
            Nothing -> []
      )

data Response a = Response String Int a deriving (Show)

instance (FromJSON a) => FromJSON (Response a) where
  parseJSON = withObject "Response" $ \v -> do
    jsonrpc <- v .: "jsonrpc"
    i <- v .: "id"
    result <- v .:? "result"
    (e :: Maybe Value) <- v .:? "error"
    case (result, e) of
      (Just r, Nothing) -> return $ Response jsonrpc i r
      (_, Just e') -> fail $ show e'
      (Nothing, Nothing) -> fail "Both result and error are missing"

data QueryData = QueryData Int Object (Maybe Object)

instance FromJSON QueryData where
  parseJSON = withObject "QueryData" $ \v ->
    QueryData
      <$> v .: "entity"
      <*> v .: "components"
      <*> v .: "has"

data SpawnResponse = SpawnResponse Int deriving (Show)

instance FromJSON SpawnResponse where
  parseJSON = withObject "SpawnResponse" $ \v -> SpawnResponse <$> v .: "entity"
