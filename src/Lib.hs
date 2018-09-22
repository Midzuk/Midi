module Lib
    ( myMidi
    ) where

import           Codec.Midi
import Data.Ratio
import Data.List
import GHC.Exts

track0 = [(10,  NoteOn 0 60 80),
          (0,  NoteOn 0 64 60),
          (10,  NoteOff 0 60 0),
          (12, NoteOff 0 64 0),
          (0,  NoteOn 0 61 60),
          (12,  NoteOff 0 61 60),
          (0,  TrackEnd)]

track1 = [(0,  NoteOn 0 65 80),
          (24, NoteOn 0 65 0),
          (0,  TrackEnd)]

track2 = unionTrack [track0, track1]

myMidi = Midi { fileType = MultiTrack,
                timeDiv  = TicksPerBeat 20,
                tracks   = [track2] }

type Length = Int                
data Sound = Sound Key Velocity Length --要修正

data Rhythm = Unit Sound | Rhythm Int Bool [Rhythm]
-- True -> 直列
-- False -> 並列

convertRhythm :: Rhythm -> Track Int
convertRhythm (Unit (Sound k v l)) =
  [ (0, NoteOn 0 k v)
  , (l, NoteOff 0 k 0)
  ]
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

unionTrack :: Real a => [Track a] -> Track a
unionTrack ts =
  let
    (ns, ms) = unzip . sortWith fst $ concat ts
  in
    zip (uncurry (-) <$> zip ns (0 : ns)) ms