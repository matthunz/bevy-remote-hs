module Main where

import Data.Bevy.Remote

main :: IO ()
main = run (query (fetch transform <* with transform)) >>= print
