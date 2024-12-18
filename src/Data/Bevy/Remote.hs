{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Bevy.Remote
  ( Remote (..),
    list,
    Component (..),
    component,
    Query (..),
    fetch,
    fetchMaybe,
    has,
    with,
    without,
    query,
    Bundle (..),
    bundle,
    spawn,
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

newtype Query a = Query {runQuery :: (Filter, Object -> Maybe Object -> Result a)}
  deriving (Functor)

instance Applicative Query where
  pure a = Query (newFilter, \_ _ -> pure a)
  Query (ss, f) <*> Query (ss', f') = Query (ss <> ss', \o hasObj -> f o hasObj <*> f' o hasObj)

fetch :: Component a -> Query a
fetch (Component name f) =
  Query
    ( newFilter {filterComponents = [name]},
      \o _ -> case (KM.lookup ((K.fromString name)) o) of
        Just x -> f x
        Nothing -> Error ("Component " ++ name ++ " not found")
    )

fetchMaybe :: Component a -> Query (Maybe a)
fetchMaybe (Component name f) =
  Query
    ( newFilter {filterOptions = [name]},
      \o _ -> case KM.lookup ((K.fromString name)) o of
        Just x -> f x >>= pure . Just
        Nothing -> pure Nothing
    )

has :: Component a -> Query (Bool)
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

data QueryData = QueryData Int Object (Maybe Object)

instance FromJSON QueryData where
  parseJSON = withObject "QueryData" $ \v ->
    QueryData
      <$> v .: "entity"
      <*> v .: "components"
      <*> v .: "has"

data QueryItem a = QueryItem Int a deriving (Show)

query :: (MonadIO m) => Query a -> Remote m [QueryItem a]
query q =
  let (fs, f) = runQuery q
   in req (QueryRequest fs) $ \res -> do
        let (Response _ _ items) = res
        return $
          mapM
            ( \(QueryData e o hasObj) ->
                case f o hasObj of
                  Error s -> Left (InvalidComponent s)
                  Success a -> Right $ QueryItem e a
            )
            items

data Bundle = Bundle (KM.KeyMap Value)

instance Monoid Bundle where
  mempty = Bundle mempty

instance Semigroup Bundle where
  Bundle a <> Bundle b = Bundle (a <> b)

instance ToJSON Bundle where
  toJSON (Bundle o) = toJSON o

bundle :: (ToJSON a) => Component a -> a -> Bundle
bundle (Component name _) a = Bundle $ KM.singleton (K.fromString name) (toJSON a)

data SpawnResponse = SpawnResponse Int deriving (Show)

instance FromJSON SpawnResponse where
  parseJSON = withObject "SpawnResponse" $ \v -> SpawnResponse <$> v .: "entity"

spawn :: (MonadIO m) => Bundle -> Remote m Int
spawn (Bundle components) =
  fmap
    (\(Response _ _ b) -> b)
    ( req
        (SpawnRequest components)
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
