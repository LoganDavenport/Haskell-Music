{-# LANGUAGE NamedFieldPuns #-}

module Key ( Sketch(..), blankSketch, sharpSketch, singletonSketch, combineSketches, inSketch, possibleKeys ) where

import qualified Accidental
import qualified Tone
import Data.List ( elemIndex )
import Data.Maybe ( fromMaybe ) 

{--
want to use patterns to generate chords, though this needs key information
key information is encoded in the root note and chord pattern

F#min for example, contains F#, A, and C#

we know it's C# and not Db because the root is F#

Cmin contains C, Eb, and G3

**coincidentally, a regular key consists of a set of 7 consecutive tones on the circle of fifths,
and the chromatic key adds the next 5!**

to determine a key, first assume that any of these are possible:
circle of fifths
                                         \/  maxSharps = 5
Gb Db Ab Eb Bb F C G D A E B F# C# G# D# A# (.. E# B#==C ) 17 tones
/\  maxFlats = 5 (key of Db)

expanding the circle of fifths to include double flats/sharps

Fbb Cbb Gbb Dbb Abb Ebb Bbb Fb Cb Gb Db Ab Eb Bb (13 flats+double flats)
F C G D A E B                                    (7 naturals)
F# C# G# D# A# E# B# F## C## G## D## A## E## B## (13 sharps+double sharps)

root tone plus which mode (for now just assume major)

want to start from the interval of "scuffed" fifth (and octave) to generate all tones

so define C0 as the base tone (like 0 \in\Z), then fifths on top of that will generate
every tone, and transposing in the interval of an octave gives all tones

the key assumtion of pythagorean equal temperment is that if you go up 12 fifths, you 
get back to the same tone that is to say,

n_k + 12*fifth = n_{k+7}

note that this is not actually true when talking about the interval of an actual perfect fifth.
two notes are said to be in an interval of an octave if their frequencies have ratio 1:2, and a 
perfect fifth if 2:3; so, since (3/2)^12 = ~129.746, which is not quite equal to the nearest 
power of 2 (128=2^7), going up by 12 perfect fifths doesn't actually get you back to the same note, 
but to one that is 7 octaves higher and slightly sharper.

to give a concrete example, following the circle of fifths above from C we have

C0 G0 D1 A1 E2 B2 F#3 C#4 G#4 D#5 A#5 E#6 B#6 (not C7 exactly, but very close)

ignoring this subtle physical detail simplifies things and gives rise to an additive interval
system rather than multiplicative.

all notes in the simplified system are generated by the interval of a semitone, which is

semitone = 7*fifth - 4*octave

the interval of an octave is implicit to when we say two tones that have different frequencies 
are the same note

the tritone sounding bad to humans is why the note a fifth below the root is taken to be
"in the key" of the root note


--}

circleOfFifths :: [Tone.T]
circleOfFifths = 
    take 35 (iterate Tone.upAFifth (Tone.ofString "Fbb"))

indexInCircle :: Tone.T -> Int
indexInCircle tone = fromMaybe (-1) (elemIndex tone circleOfFifths)


data Sketch = Sketch {
    low :: Int,
    high :: Int
} deriving (Eq, Show, Read)

sketch :: Int -> Int -> Sketch
sketch low high = Sketch { low, high }

blankSketch :: Sketch
blankSketch = sketch 0 (length circleOfFifths)

sharpSketch = sketch 14 26

singletonSketch :: Tone.T -> Sketch
singletonSketch (_, Accidental.DoubleSharp) = blankSketch
singletonSketch (_, Accidental.DoubleFlat)  = blankSketch
singletonSketch tone =
    let index = indexInCircle tone
    in
        Sketch { low = max 0 (index-6), high = min (length circleOfFifths) (index+7) }


combineSketches :: Sketch -> Sketch -> Sketch
combineSketches s1 s2 = sketch (max (low s1) (low s2)) (min (high s1) (high s2))

inSketch :: Sketch -> Tone.T -> Bool
inSketch s tone =
    let index = indexInCircle tone
    in
        low s <= index && index < high s

possibleKeys :: [Tone.T] -> [Tone.T]
possibleKeys tones = 
    let finalSketch = foldl (\acc tone -> combineSketches acc (singletonSketch tone)) blankSketch tones
    in
        drop (low finalSketch + 1) (take (high finalSketch - 5) circleOfFifths)