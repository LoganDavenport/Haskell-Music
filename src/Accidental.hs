module Accidental ( T(..), toDelta, ofString, toString, allAccidentals) where
    
data T = DoubleFlat | Flat | Natural | Sharp | DoubleSharp deriving (Eq, Read, Show)

allAccidentals :: [T]
allAccidentals = [ DoubleFlat, Flat, Natural, Sharp, DoubleSharp ]

toDelta :: T -> Int
toDelta DoubleFlat  = -2
toDelta Flat        = -1
toDelta Natural     = 0
toDelta Sharp       = 1
toDelta DoubleSharp = 2

ofString :: String -> T
ofString "bb"   = DoubleFlat
ofString "b"    = Flat
ofString ""     = Natural
ofString "s"    = Sharp
ofString "#"    = Sharp
ofString "x"    = DoubleSharp
ofString _      = undefined

-- this could take context to determine if the accidental should be displayed
toString :: T -> String
toString DoubleFlat     = "bb"
toString Flat           = "b"
toString Natural        = ""
toString Sharp          = "#"
toString DoubleSharp    = "x"
