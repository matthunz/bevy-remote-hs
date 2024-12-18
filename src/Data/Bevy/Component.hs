{-# LANGUAGE OverloadedStrings #-}

module Data.Bevy.Component
  ( Transform (..),
    transform,
    Visibility (..),
    visibility,
  )
where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Bevy.Remote
import qualified Data.Vector as V
import Linear

data Transform = Transform
  { transformScale :: V3 Float,
    transformTranslation :: V3 Float,
    transformRotation :: V4 Float
  }
  deriving (Show)

instance FromJSON Transform where
  parseJSON = withObject "Transform" $ \v -> do
    scale <- v .: "scale" >>= parseV3
    t <- v .: "translation" >>= parseV3
    rotation <- v .: "rotation" >>= parseV4
    return $ Transform scale t rotation

transform :: Component Transform
transform = component "bevy_transform::components::transform::Transform"

data Visibility = Inherited | Visible | Hidden | Collapsed
  deriving (Show)

instance FromJSON Visibility where
  parseJSON = withText "Visibility" $ \s -> case s of
    "Inherited" -> return Inherited
    "Visible" -> return Visible
    "Hidden" -> return Hidden
    "Collapsed" -> return Collapsed
    _ -> fail "Invalid visibility"

visibility :: Component Visibility
visibility = component "bevy_render::view::visibility::Visibility"

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
