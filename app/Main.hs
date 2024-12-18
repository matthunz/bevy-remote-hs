module Main where

import qualified Data.Bevy.Component as C
import Data.Bevy.Remote

cube :: Component Float
cube = component "server::Cube"

main :: IO ()
main =
  run
    ( query $
        (,)
          <$> fetch C.transform
          <*> has cube
          <* with C.visibility
    )
    >>= print
