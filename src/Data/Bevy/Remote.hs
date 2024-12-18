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
import Network.HTTP.Client
  ( RequestBody (RequestBodyLBS),
    defaultManagerSettings,
    httpLbs,
    newManager,
    parseRequest,
  )
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (methodPost)

data QueryRequestData = QueryRequestData
  { queryComponents :: [String],
    queryOptions :: [String],
    queryHas :: [String],
    queryWith :: [String],
    queryWithout :: [String]
  }
  deriving (Show)

data RequestKind = ListRequest | QueryRequest QueryRequestData deriving (Show)

data Request = Request Int RequestKind deriving (Show)

instance ToJSON Request where
  toJSON (Request i ListRequest) =
    object
      [ "jsonrpc" .= ("2.0" :: String),
        "id" .= i,
        "method" .= ("bevy/list" :: String)
      ]
  toJSON (Request i (QueryRequest d)) =
    object
      [ "jsonrpc" .= ("2.0" :: String),
        "id" .= i,
        "method" .= ("bevy/query" :: String),
        "params"
          .= object
            [ "data" .= object ["components" .= queryComponents d],
              "filter" .= object ["with" .= queryWith d, "without" .= queryWithout d]
            ]
      ]

data Response a = Response String Int a deriving (Show)

instance (FromJSON a) => FromJSON (Response a) where
  parseJSON = withObject "Response" $ \v -> Response <$> v .: "jsonrpc" <*> v .: "id" <*> v .: "result"

data Error = InvalidResponse String | InvalidComponent String deriving (Show)

data Remote m a = Remote {runRemote :: HTTP.Manager -> String -> Int -> m (Either Error a)}
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

req :: (MonadIO m, FromJSON a) => RequestKind -> (Response a -> m (Either Error b)) -> Remote m b
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

data Component a = Component String (Object -> Result a)

component :: (FromJSON a) => String -> Component a
component name = Component name (fromJSON . Object)

data Transform = Transform deriving (Show)

instance FromJSON Transform where
  parseJSON = withObject "Transform" $ \_ -> pure Transform

transform :: Component Transform
transform = component "bevy_transform::components::transform::Transform"

data Filter = ComponentFilter String | WithFilter String | WithoutFilter String deriving (Show)

newtype Query a = Query {runQuery :: ([Filter], Object -> Result a)}
  deriving (Functor)

instance Applicative Query where
  pure a = Query ([], \_ -> pure a)
  Query (ss, f) <*> Query (ss', f') = Query (ss ++ ss', \o -> f o <*> f' o)

fetch :: Component a -> Query a
fetch (Component name f) = Query ([ComponentFilter name], f)

filterQuery :: Filter -> Query ()
filterQuery f = Query ([f], \_ -> pure ())

with :: Component a -> Query ()
with (Component name _) = filterQuery (WithFilter name)

without :: Component a -> Query ()
without (Component name _) = filterQuery (WithoutFilter name)

data QueryData = QueryData Int Object

instance FromJSON QueryData where
  parseJSON = withObject "QueryData" $ \v -> QueryData <$> v .: "entity" <*> v .: "components"

data QueryItem a = QueryItem Int a deriving (Show)

filterData :: QueryRequestData -> Filter -> QueryRequestData
filterData d (ComponentFilter name) = d {queryComponents = name : queryComponents d}
filterData d (WithFilter name) = d {queryWith = name : queryWith d}
filterData d (WithoutFilter name) = d {queryWithout = name : queryWithout d}

query :: (MonadIO m) => Query a -> Remote m [QueryItem a]
query q =
  let (fs, f) = runQuery q
      d = foldl filterData (QueryRequestData [] [] [] [] []) fs
   in req (QueryRequest d) $ \res -> do
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
