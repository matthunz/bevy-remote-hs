module Main where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bevy.Remote

cube :: Component ()
cube = component "server::Cube"

main :: IO ()
main =
  run
    ( do
        list >>= liftIO . print

        query (fetch transform <* with cube) >>= liftIO . print
    )
    >>= print
