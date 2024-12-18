{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Bevy.Remote
  ( -- * Component
    Component (..),
    component,

    -- * Bundle
    Bundle (..),
    bundle,

    -- * Remote
    Remote (..),
    request,
    list,
    query,
    spawn,

    -- * Query
    Query (..),
    fetch,
    fetchMaybe,
    has,
    with,
    without,

    -- * Client
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
import qualified Data.Aeson.KeyMap as KM
import Data.Bevy.Remote.Transport
import Network.HTTP.Client
  ( RequestBody (RequestBodyLBS),
    defaultManagerSettings,
    httpLbs,
    newManager,
    parseRequest,
  )
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (methodPost)

data Component a = Component String (Value -> Result a)

-- | Create a component marker from its ID.
component :: (FromJSON a) => String -> Component a
component name = Component name fromJSON

-- | Bundle of components.
data Bundle = Bundle (KM.KeyMap Value)

instance Monoid Bundle where
  mempty = Bundle mempty

instance Semigroup Bundle where
  Bundle a <> Bundle b = Bundle (a <> b)

instance ToJSON Bundle where
  toJSON (Bundle o) = toJSON o

-- | Create a bundle from a component.
bundle :: (ToJSON a) => Component a -> a -> Bundle
bundle (Component name _) a = Bundle $ KM.singleton (K.fromString name) (toJSON a)

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

-- | Send a BRP request to the server.
request ::
  (ToJSON c, MonadIO m, FromJSON a) =>
  String ->
  Maybe c ->
  (Response a -> m (Either Error b)) ->
  Remote m b
request method r f = Remote $ \manager url i -> do
  let json = encode $ toJSON (Request method i r)
  initialRequest <- liftIO $ parseRequest url
  let req =
        initialRequest
          { HTTP.method = methodPost,
            HTTP.requestHeaders = [("Content-Type", "application/json")],
            HTTP.requestBody = RequestBodyLBS json
          }
  response <- liftIO $ httpLbs req manager
  case (eitherDecode (HTTP.responseBody response)) of
    Left e -> return $ Left (InvalidResponse e)
    Right res -> f res

-- | List all spawned entities.
list :: (MonadIO m) => Remote m [String]
list = fmap (\(Response _ _ a) -> a) (request "bevy/list" (Nothing :: Maybe ()) (\res -> return $ pure res))

newtype Query a = Query {runQuery :: (Filter, Object -> Maybe Object -> Result a)}
  deriving (Functor)

instance Applicative Query where
  pure a = Query (newFilter, \_ _ -> pure a)
  Query (ss, f) <*> Query (ss', f') = Query (ss <> ss', \o hasObj -> f o hasObj <*> f' o hasObj)

-- | Fetch a component from a spawned entity.
fetch :: Component a -> Query a
fetch (Component name f) =
  Query
    ( newFilter {filterComponents = [name]},
      \o _ -> case (KM.lookup ((K.fromString name)) o) of
        Just x -> f x
        Nothing -> Error ("Component " ++ name ++ " not found")
    )

-- | Optionally fetch a component from a spawned entity.
fetchMaybe :: Component a -> Query (Maybe a)
fetchMaybe (Component name f) =
  Query
    ( newFilter {filterOptions = [name]},
      \o _ -> case KM.lookup ((K.fromString name)) o of
        Just x -> f x >>= pure . Just
        Nothing -> pure Nothing
    )

-- |
-- Query for the existence of a component on a spawned entity,
-- returning `True` if it is present.
has :: Component a -> Query Bool
has (Component name _) =
  Query
    ( newFilter {filterHas = [name]},
      \_ o -> case o >>= KM.lookup ((K.fromString name)) of
        Just (Bool b) -> pure b
        _ -> Error "Expected a boolean value for `has` component."
    )

filterQuery :: Filter -> Query ()
filterQuery f = Query (f, \_ _ -> pure ())

with :: Component a -> Query ()
with (Component name _) = filterQuery (newFilter {filterWith = [name]})

without :: Component a -> Query ()
without (Component name _) = filterQuery (newFilter {filterWithout = [name]})

data QueryItem a = QueryItem Int a deriving (Show)

-- | Query the ECS, returning a list of matching items.
query :: (MonadIO m) => Query a -> Remote m [QueryItem a]
query q =
  let (fs, f) = runQuery q
   in request "bevy/query" (Just (QueryRequest fs)) $ \res -> do
        let (Response _ _ items) = res
        return $
          mapM
            ( \(QueryData e o hasObj) ->
                case f o hasObj of
                  Error s -> Left (InvalidComponent s)
                  Success a -> Right $ QueryItem e a
            )
            items

-- | Spawn a bundle of components.
spawn :: (MonadIO m) => Bundle -> Remote m Int
spawn (Bundle components) =
  fmap
    (\(Response _ _ b) -> b)
    ( request
        "bevy/spawn"
        (Just $ SpawnRequest components)
        (\(Response s i (SpawnResponse e)) -> return $ pure (Response s i e))
    )

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
