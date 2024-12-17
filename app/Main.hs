module Main where

import Data.Bevy.Remote

main :: IO ()
main = run (query (fetch :: Query Transform)) >>= print
