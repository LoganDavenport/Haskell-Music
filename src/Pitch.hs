{-- how exactly pitches are implemented is not entirely important, since we're most interested
in intervals between notes, so representing them as semitones above c0 is fine.

this does bake in the assumption of some sort of equal temperament though, but it's possible to
play with how many notes are in the scale which could be interesting 
(i.e. don't assume 12 fifths is an octave) 
--}
module Pitch ( T ) where

{-- how exactly the notes are implemented is not entirely important, since we're most interested
in intervals between notes, so representing notes as semitones above c0 is fine.

    this actually loses key information which was part of what made note nice and useful for determining key
    
    if we base keys off of fifths distance, that's fine thought

this does bake in the assumption of some sort of equal temperament though, but it's possible to
play with how many notes are in the scale which could be interesting --}
type T = Int

c0 :: T
c0 = 0

gb0 :: T
gb0 = 6