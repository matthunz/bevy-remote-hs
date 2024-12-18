# bevy-remote-hs

A Haskell library for interfacing with the [Bevy](https://github.com/bevyengine/bevy) game engine.
This implements the [Bevy Remote Protocol (BRP)](https://docs.rs/bevy/latest/bevy/remote/index.html).

```hs
import Data.Bevy.Remote

cube :: Component ()
cube = component "server::Cube"

main :: IO ()
main =
  run
    ( query
        ( (,)
            <$> fetch C.transform
            <*> has cube
            <* with C.visibility
        )
    )
    >>= print
```
