module Main where

import Data.Bevy.Remote

main :: IO ()
main = run list >>= print
