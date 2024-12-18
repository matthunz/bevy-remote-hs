{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Bevy.Remote
  ( Remote (..),
    list,
    Component (..),
    component,
    Transform (..),
    transform,
    Query (..),
    fetch,
    fetchMaybe,
    with,
    without,
    query,
    Client (..),
    newClient,
    newClientWith,
    runClient,
    run,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as HM
import Data.Aeson.Types (Parser)
import qualified Data.Vector as V
import Linear
import Network.HTTP.Client
  ( RequestBody (RequestBodyLBS),
    defaultManagerSettings,
    httpLbs,
    newManager,
    parseRequest,
  )
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (methodPost)

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

data RequestKind = ListRequest | QueryRequest Filter deriving (Show)

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
                            "option" .= filterOptions d
                          ],
                      "filter"
                        .= object
                          [ "with" .= filterWith d,
                            "without" .= filterWithout d
                          ]
                    ]
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
  parseJSON = withObject "Response" $ \v ->
    Response <$> v .: "jsonrpc" <*> v .: "id" <*> v .: "result"

data Error = InvalidResponse String | InvalidComponent String deriving (Show)

newtype Remote m a = Remote {runRemote :: HTTP.Manager -> String -> Int -> m (Either Error a)}
  deriving (Functor)

instance (Monad m) => Applicative (Remote m) where
  pure a = Remote $ \_ _ _ -> pure (pure a)
  Remote f <*> Remote a = Remote $ \m u i -> do
    f' <- f m u i
    a' <- a m u i
    return $ f' <*> a'

instance (Monad m) => Monad (Remote m) where
  Remote a >>= f = Remote $ \m u i -> do
    a' <- a m u i
    case a' of
      Left e -> return $ Left e
      Right a'' -> runRemote (f a'') m u i

instance (MonadIO m) => MonadIO (Remote m) where
  liftIO a = Remote $ \_ _ _ -> liftIO $ fmap pure a

req ::
  (MonadIO m, FromJSON a) =>
  RequestKind ->
  (Response a -> m (Either Error b)) ->
  Remote m b
req r f = Remote $ \manager url i -> do
  let json = encode $ toJSON (Request i r)
  initialRequest <- liftIO $ parseRequest url
  let request =
        initialRequest
          { HTTP.method = methodPost,
            HTTP.requestHeaders = [("Content-Type", "application/json")],
            HTTP.requestBody = RequestBodyLBS json
          }
  response <- liftIO $ httpLbs request manager
  case (eitherDecode (HTTP.responseBody response)) of
    Left e -> return $ Left (InvalidResponse e)
    Right res -> f res

list :: (MonadIO m) => Remote m [String]
list = fmap (\(Response _ _ a) -> a) (req ListRequest (\res -> return $ pure res))

data Component a = Component String (Value -> Result a)

component :: (FromJSON a) => String -> Component a
component name = Component name fromJSON

data Transform = Transform
  { transformScale :: V3 Float,
    transformTranslation :: V3 Float,
    transformRotation :: V4 Float
  }
  deriving (Show)

parseV3 :: (FromJSON a) => Value -> Parser (V3 a)
parseV3 (Array v) = do
  let e1 = v V.!? 0
      e2 = v V.!? 1
      e3 = v V.!? 2
  case (e1, e2, e3) of
    (Just v1, Just v2, Just v3) ->
      V3
        <$> parseJSON v1
        <*> parseJSON v2
        <*> parseJSON v3
    _ -> fail "Expected an array of exactly 3 elements"
parseV3 _ = fail "Expected a JSON array of length 3"

parseV4 :: (FromJSON a) => Value -> Parser (V4 a)
parseV4 (Array v) = do
  let e1 = v V.!? 0
      e2 = v V.!? 1
      e3 = v V.!? 2
      e4 = v V.!? 3
  case (e1, e2, e3, e4) of
    (Just v1, Just v2, Just v3, Just v4) ->
      V4
        <$> parseJSON v1
        <*> parseJSON v2
        <*> parseJSON v3
        <*> parseJSON v4
    _ -> fail "Expected an array of exactly 4 elements"
parseV4 _ = fail "Expected a JSON array of length 4"

instance FromJSON Transform where
  parseJSON = withObject "Transform" $ \v -> do
    scale <- v .: "scale" >>= parseV3
    t <- v .: "translation" >>= parseV3
    rotation <- v .: "rotation" >>= parseV4
    return $ Transform scale t rotation

transform :: Component Transform
transform = component "bevy_transform::components::transform::Transform"

newtype Query a = Query {runQuery :: (Filter, Object -> Result a)}
  deriving (Functor)

instance Applicative Query where
  pure a = Query (newFilter, \_ -> pure a)
  Query (ss, f) <*> Query (ss', f') = Query (ss <> ss', \o -> f o <*> f' o)

fetch :: Component a -> Query a
fetch (Component name f) =
  Query
    ( newFilter {filterComponents = [name]},
      \o -> case (HM.lookup ((K.fromString name)) o) of
        Just x -> f x
        Nothing -> Error ("Component " ++ name ++ " not found")
    )

fetchMaybe :: Component a -> Query (Maybe a)
fetchMaybe (Component name f) =
  Query
    ( newFilter {filterOptions = [name]},
      \o -> case HM.lookup ((K.fromString name)) o of
        Just x -> f x >>= pure . Just
        Nothing -> pure Nothing
    )

filterQuery :: Filter -> Query ()
filterQuery f = Query (f, \_ -> pure ())

with :: Component a -> Query ()
with (Component name _) = filterQuery (newFilter {filterWith = [name]})

without :: Component a -> Query ()
without (Component name _) = filterQuery (newFilter {filterWithout = [name]})

data QueryData = QueryData Int Object

instance FromJSON QueryData where
  parseJSON = withObject "QueryData" $ \v -> QueryData <$> v .: "entity" <*> v .: "components"

data QueryItem a = QueryItem Int a deriving (Show)

query :: (MonadIO m) => Query a -> Remote m [QueryItem a]
query q =
  let (fs, f) = runQuery q
   in req (QueryRequest fs) $ \res -> do
        let (Response _ _ items) = res
        return $
          mapM
            ( \(QueryData e o) ->
                case f o of
                  Error s -> Left (InvalidComponent s)
                  Success a -> Right $ QueryItem e a
            )
            items

data Client = Client
  { clientManager :: HTTP.Manager,
    clientURL :: String
  }

newClientWith :: HTTP.Manager -> Client
newClientWith m = Client m "http://localhost:15702"

newClient :: IO Client
newClient = do
  manager <- newManager defaultManagerSettings
  return $ newClientWith manager

runClient :: Remote m a -> Client -> m (Either Error a)
runClient r (Client manager url) = runRemote r manager url 1

run :: (MonadIO m) => Remote m a -> m (Either Error a)
run r = liftIO newClient >>= runClient r
