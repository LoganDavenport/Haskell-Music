{-# LANGUAGE NamedFieldPuns #-}


module Note ( T(..), putAbove, pitch, ofPitch, ofString, toString, diff ) where

import qualified Pitch
import qualified Tone
import qualified Key

-- tone with octave
data T = T {
    tone :: Tone.T,
    octave :: Int
} deriving (Eq, Read, Show)

pitch :: T -> Pitch.T
pitch t = 
    Tone.lowestOctave (tone t) + 12 * octave t

ofPitch :: Key.Sketch -> Pitch.T -> [T]
ofPitch sketch p = 
        map (\tone -> T { tone, octave = quot p 12 }) 
            (filter (Key.inSketch sketch) (Tone.ofPitch p))

ofString :: String -> T
ofString string = 
    let tone = Tone.ofString (init string)
        octave = read [last string] :: Int
    in
        T { tone, octave }

toString :: T -> String
toString t =
    Tone.toString (tone t) ++ show (octave t)

diff :: T -> T -> Int
diff n1 n2 = pitch n1 - pitch n2

instance Ord T where
    compare n1 n2 | d > 0  = GT
                  | d == 0 = EQ
                  | otherwise = LT
        where 
            d = diff n1 n2

putAbove :: Tone.T -> T -> T
putAbove tone above = 
    let noteAt deltaO = T { tone = tone, octave = deltaO + octave above }
        isAbove n = n > above
    in
     head (filter isAbove (map noteAt [0, 1, 2]))
