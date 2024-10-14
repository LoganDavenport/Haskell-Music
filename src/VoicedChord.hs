
module VoicedChord ( T, size, firstInvertion, c5, classify, addShape ) where

import qualified Note
import qualified Tone
import qualified Key
import qualified Data.Set
import qualified Data.List
import qualified Data.Maybe

type T = Data.Set.Set Note.T

size :: T -> Int
size = Data.Set.size

firstInvertion :: T -> T
firstInvertion t =
    let (bassNote, rest) = Data.Set.deleteFindMin t
        highestNote = Data.Set.findMax t
    in
        Data.Set.insert (Note.putAbove (Note.tone bassNote) highestNote) rest


chord :: [String] -> T
chord strings = Data.Set.fromList (map Note.ofString strings)

c5 :: T
c5 = chord [ "C3", "G3" ]

goverdb :: T
goverdb = chord [ "Db3", "G3", "B3", "D4"]

c6 :: T
c6 = chord [ "C3", "E3", "G3", "A3" ]

{--

chord patterns to classify chords based on what tones appear in them

first, determine the bass note using [Data.Set.findMin]. 
this note may appear in the chord (in the case of D/F# or [c5] above)
or may not (in a chord like G/Db)
assume it does, and if that fits an easy pattern then epic otherwise take it out

how to represent these patterns?

probably just as sets of intervals above the bass plus all inversions

the pattern of a 5 chord is the 1 and P5
the pattern of a major chord is the 1 M3 and P5


all patterns are sets of intervals from [rootNote]
    (which may or may not equal [bassNote])

notes:
- C6 should be identified as such and not as Amin7/C

could just encode the info in the classify function, e.g. matching on if the M3 and P5 are in a chord



--}

type ChordShape = (String, Data.Set.Set Int)

patt :: String -> [Int] -> ChordShape
patt name intervals = (name, Data.Set.fromList intervals)


power :: ChordShape
power = patt "5" [7]
maj :: ChordShape
maj = patt "maj" [4,7]
m :: ChordShape
m = patt "m" [3,7]
dom7 :: ChordShape
dom7 = patt "dom7" [4,7,10]
maj7 :: ChordShape
maj7 = patt "maj7" [4,7,11]
min7 :: ChordShape
min7 = patt "min7" [3,7,10]
maj6 :: ChordShape
maj6 = patt "maj6" [4,7,9]
min6 :: ChordShape
min6 = patt "min6" [3,7,9]


majorScale :: ChordShape
majorScale = patt "majorScale" [ 2, 4, 5, 7, 9, 11 ]

allPatterns :: [ChordShape]
allPatterns =
    [ maj7, min7, maj6, min6, dom7, maj, m, power]

hasShape :: T -> ChordShape -> Bool
hasShape t (_, neededIntervals) =
    let (assumedRoot, rest) = Data.Set.deleteFindMin t
        toInterval note = mod (Note.diff note assumedRoot) 12
        intervals = Data.Set.map toInterval rest
    in
     Data.Set.isSubsetOf (Data.Set.map (`mod` 12) neededIntervals) intervals

{-- returns the tones that, when treated as the root note, make the chord have the given shape
--}
findRoot :: T -> ChordShape -> [Tone.T]
findRoot t chordShape =
    let getInversions ch 1 = [ch]
        getInversions ch n = ch : getInversions (firstInvertion ch) (n-1)
        inversionsWithShape = filter (`hasShape` chordShape) (getInversions t (size t))
    in
        map (Note.tone . Data.Set.findMin) inversionsWithShape


classify :: T -> [String]
classify t =
    let bassTone = Note.tone (Data.Set.findMin t)
        tryPattern pattern = map (,pattern) (findRoot t pattern)
        allMatches = concatMap tryPattern allPatterns
        (rootEqualsBass, notEqual) = Data.List.partition (\(root, _) -> root == bassTone) allMatches
        toneAndPatternToString (root, (chordName, _)) = 
            let bassString | root == bassTone = ""
                           | otherwise        = "/" ++ Tone.toString bassTone
            in
            Tone.toString root ++ chordName ++ bassString
    in
       map toneAndPatternToString (rootEqualsBass ++ notEqual)


addShape :: Note.T -> ChordShape -> T
addShape root (_, intervals) = 
    let sketch = Key.singletonSketch (Note.tone root)
        noteOfInterval interval = head (Note.ofPitch sketch 
                                            (Note.pitch root + interval))
        upperNotes = Data.Set.map noteOfInterval intervals
    in
        Data.Set.insert root upperNotes