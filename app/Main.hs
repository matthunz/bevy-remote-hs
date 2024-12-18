module Main where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Bevy.Component as C
import Data.Bevy.Remote

cube :: Component ()
cube = component "server::Cube"

main :: IO ()
main =
  run
    ( do
        list >>= liftIO . print

        query
          ( (,)
              <$> fetch C.transform
              <*> fetchMaybe C.transform
              <* with cube
          )
          >>= liftIO . print
    )
    >>= print
