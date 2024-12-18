# bevy-remote-hs

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
