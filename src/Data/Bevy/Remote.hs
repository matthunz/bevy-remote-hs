{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Bevy.Remote
  ( Remote (..),
    list,
    Component (..),
    Transform (..),
    transform,
    Query (..),
    fetch,
    query,
    run,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
import Network.HTTP.Client
  ( RequestBody (RequestBodyLBS),
    defaultManagerSettings,
    httpLbs,
    newManager,
    parseRequest,
  )
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (methodPost)

data RequestKind = ListRequest | QueryRequest [String] deriving (Show)

data Request = Request Int RequestKind deriving (Show)

instance ToJSON Request where
  toJSON (Request i ListRequest) =
    object
      [ "jsonrpc" .= ("2.0" :: String),
        "id" .= i,
        "method" .= ("bevy/list" :: String)
      ]
  toJSON (Request i (QueryRequest cs)) =
    object
      [ "jsonrpc" .= ("2.0" :: String),
        "id" .= i,
        "method" .= ("bevy/query" :: String),
        "params" .= object ["data" .= object ["components" .= cs]]
      ]

data Response a = Response String Int a deriving (Show)

instance (FromJSON a) => FromJSON (Response a) where
  parseJSON = withObject "Response" $ \v -> Response <$> v .: "jsonrpc" <*> v .: "id" <*> v .: "result"

data Remote m a = Remote {runRemote :: HTTP.Manager -> String -> Int -> m a}

instance (Functor m) => Functor (Remote m) where
  fmap f (Remote r) = Remote $ \manager url i -> fmap f (r manager url i)

req :: (MonadIO m, FromJSON a) => RequestKind -> Remote m (Either String (Response a))
req r = Remote $ \manager url i -> liftIO $ do
  let json = encode $ toJSON (Request i r)
  initialRequest <- parseRequest url
  let request =
        initialRequest
          { HTTP.method = methodPost,
            HTTP.requestHeaders = [("Content-Type", "application/json")],
            HTTP.requestBody = RequestBodyLBS json
          }
  response <- httpLbs request manager
  return $ eitherDecode (HTTP.responseBody response)

list :: (MonadIO m) => Remote m (Either String [String])
list = fmap (\res -> fmap (\(Response _ _ a) -> a) res) (req ListRequest)

data Component a = Component String (Object -> Result a)

component :: (FromJSON a) => String -> Component a
component name = Component name (fromJSON . Object)

data Transform = Transform deriving (Show)

instance FromJSON Transform where
  parseJSON = withObject "Transform" $ \_ -> pure Transform

transform :: Component Transform
transform = component "bevy_transform::components::transform::Transform"

newtype Query a = Query {runQuery :: ([String], Object -> Result a)}
  deriving (Functor)

instance Applicative Query where
  pure a = Query ([], \_ -> pure a)
  Query (ss, f) <*> Query (ss', f') = Query (ss ++ ss', \o -> f o <*> f' o)

fetch :: Component a -> Query a
fetch (Component name f) = Query ([name], f)

data QueryData = QueryData Int Object

instance FromJSON QueryData where
  parseJSON = withObject "QueryData" $ \v -> QueryData <$> v .: "entity" <*> v .: "components"

data QueryItem a = QueryItem Int a deriving (Show)

query :: (MonadIO m) => Query a -> Remote m (Either String ([Result (QueryItem a)]))
query q =
  let (names, f) = runQuery q
   in fmap (\res -> fmap (\(Response _ _ items) -> map (\(QueryData i o) -> fmap (QueryItem i) (f o)) items) res) (req (QueryRequest names))

run :: Remote IO a -> IO a
run r = do
  manager <- newManager defaultManagerSettings
  let url = "http://localhost:15702"
  runRemote r manager url 1
