module Lib
    ( myMidi
    ) where

import           Codec.Midi


track0 = [(0,  NoteOn 0 60 80),
          (1,  NoteOff 0 60 0),
          (0,  NoteOn 0 64 60),
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
