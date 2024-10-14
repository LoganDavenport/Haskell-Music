
module Frequency ( T, c4, f3, halfStep, centsDifference ) where

type T = Float

c4 :: T
c4 = 261.626

f3 :: T
f3 = 174.614

halfStep :: Float
halfStep = 2**(1/12)

centsDifference :: T -> T -> Float
centsDifference f1 f2 = 100 * logBase halfStep (f1/f2)