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

data RequestKind = ListRequest | QueryRequest Filter | SpawnRequest (KM.KeyMap Value) deriving (Show)

data Request = Request Int RequestKind deriving (Show)

instance ToJSON Request where
  toJSON (Request i r) =
    let (method, entries) = case r of
          ListRequest -> ("bevy/list" :: String, [])
          QueryRequest d ->
            ( "bevy/query" :: String,
              [ "params"
                  .= object
                    [ "data"
                        .= object
                          [ "components" .= filterComponents d,
                            "option" .= filterOptions d,
                            "has" .= filterHas d
                          ],
                      "filter"
                        .= object
                          [ "with" .= filterWith d,
                            "without" .= filterWithout d
                          ]
                    ]
              ]
            )
          SpawnRequest obj ->
            ( "bevy/spawn" :: String,
              [ "params"
                  .= object ["components" .= obj]
              ]
            )
     in object
          ( [ "jsonrpc" .= ("2.0" :: String),
              "id" .= i,
              "method" .= method
            ]
              ++ entries
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
