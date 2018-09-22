module Lib
    ( myMidi
    ) where

import           Codec.Midi
import Data.Ratio
import Data.List

track0 = [(0,  NoteOn 0 60 80),
          (0,  NoteOn 0 64 60),
          (1,  NoteOff 0 60 0),
          (12, NoteOff 0 64 0),
          (0,  NoteOn 0 61 60),
          (12,  NoteOff 0 61 60),
          (0,  TrackEnd)]

track1 = [(0,  NoteOn 0 65 80),
          (24, NoteOn 0 65 0),
          (0,  TrackEnd)]

myMidi = Midi { fileType = MultiTrack,
                timeDiv  = TicksPerBeat 240,
                tracks   = [track0, track1] }

data Sound = Sound Int

data Rhythm = Unit Sound | Rhythm Int Bool [Rhythm]
-- True -> 直列
-- False -> 並列

{-
--Rhythmの数値化
convertRhythm :: Rhythm -> (Int, [(Ratio Int, Sound)])
convertRhythm (Unit s) = (1, [(1 % 1, s)])
convertRhythm (Rhythm n False rs) = (n, concat $ snd . convertRhythm <$> rs)
convertRhythm (Rhythm n True rs) =
  let
    xs = convertRhythm <$> rs
    ys = f <$> xs
  where
    f (n_, )
  (n, concat $ snd . convertRhythm <$> rs)
-}