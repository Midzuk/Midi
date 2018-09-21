module Main where

import Lib
import           Codec.Midi


main :: IO ()
main = exportFile "my-midi.mid" myMidi
