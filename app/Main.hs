module Main where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Bevy.Component as C
import Data.Bevy.Remote

cube :: Component Float
cube = component "server::Cube"

main :: IO ()
main =
  run
    ( do
        list >>= liftIO . print

        spawn (bundle cube 1) >>= liftIO . print

        query
          ( (,)
              <$> fetch C.transform
              <*> has cube
              <* with cube
          )
          >>= liftIO . print
    )
    >>= print
