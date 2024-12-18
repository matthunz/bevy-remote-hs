# bevy-remote-hs

A Haskell library for interfacing with the [Bevy](https://github.com/bevyengine/bevy) game engine.
This implements the [Bevy Remote Protocol (BRP)](https://docs.rs/bevy/latest/bevy/remote/index.html).

```hs
import qualified Data.Bevy.Component as C
import Data.Bevy.Remote

-- Create a `cube` component to access this type from Bevy's ECS
cube :: Component Float
cube = component "server::Cube"

-- Query the ECS, printing any matching items
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

## Getting started
You can run Bevy with the remote server enabled by running the [official example](https://github.com/bevyengine/bevy/blob/main/examples/remote/server.rs).

```
git clone https://github.com/bevyengine/bevy
cd bevy
cargo run --example server --features bevy_remote
```

