{-# LANGUAGE OverloadedStrings #-}

module Data.Bevy.Remote (Remote (..), list, run) where

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

data RequestKind = ListRequest deriving (Show)

data Request = Request Int RequestKind deriving (Show)

instance ToJSON Request where
  toJSON (Request i ListRequest) =
    object
      [ "jsonrpc" .= ("2.0" :: String),
        "id" .= i,
        "method" .= ("bevy/list" :: String)
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

run :: Remote IO a -> IO a
run r = do
  manager <- newManager defaultManagerSettings
  let url = "http://localhost:15702"
  runRemote r manager url 1
