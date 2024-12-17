module Main where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bevy.Remote

main :: IO ()
main =
  run
    ( do
        list >>= liftIO . print

        query (fetch transform <* with transform) >>= liftIO . print
    )
    >>= print
