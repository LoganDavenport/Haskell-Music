
{--- used for naming notes ---}
module Tone ( T, ScaleTone(..), upAFifth, lowestOctave, ofPitch, ofString, toString ) where

import qualified Accidental
import qualified Pitch
import Data.Char (toUpper)
import Data.List ( elemIndex, find )
import Data.Maybe ( fromMaybe ) 

data ScaleTone = A | B | C | D | E | F | G deriving (Eq, Read, Show, Ord)


allScaleTones :: [ScaleTone]
allScaleTones = map (\char -> read [char] :: Tone.ScaleTone) "ABCDEFG"

upFrom :: Int -> ScaleTone -> ScaleTone
upFrom n scaleTone = 
    let index = fromMaybe (-1) (elemIndex scaleTone allScaleTones)
    in
        allScaleTones!!mod (index+n) (length allScaleTones)

aboveCBy :: ScaleTone -> Int
aboveCBy C = 0
aboveCBy D = 2
aboveCBy E = 4
aboveCBy F = 5
aboveCBy G = 7
aboveCBy A = 9
aboveCBy B = 11

type T = (ScaleTone, Accidental.T)

allTones :: [T]
allTones = [(t,a) | t <- allScaleTones, a <- Accidental.allAccidentals]

lowestOctave :: T -> Pitch.T
lowestOctave (scaleTone, accidental) = mod (aboveCBy scaleTone + Accidental.toDelta accidental) 12

{-- this could be precomputed easily --}
ofPitch :: Pitch.T -> [T]
ofPitch p = filter ((== rem p 12) . lowestOctave) allTones

ofString :: String -> T
ofString string = 
    let scaleTone = read [toUpper (head string)]
        accidental = Accidental.ofString (tail string)
        in
            (scaleTone, accidental)

toString :: T -> String
toString (scaleTone, accidental) = show scaleTone ++ Accidental.toString accidental

{-- to go up a fifth, simply go up 4 scale tones and choose the appropriate accidental to make the 
    difference 7 tones. alternatively, you could just go up 7 semitones. since [ofPitch] is implemented
    above the second approach is easiest.
--}
upAFifth :: T -> T
upAFifth t =
    let (scaleTone, _) = t
        options = ofPitch (lowestOctave t + 7)
        fifthScaleTone = 4 `upFrom` scaleTone
    in
       ofString "-1" `fromMaybe` find (\(st, _) -> st == fifthScaleTone) options


