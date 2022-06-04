
module XenFret (
    makeFret,
    changeScale,
    repeatingNotes,
    board,
    Fretboard()
) where

import Diagrams.Attributes
import Control.Monad
import Diagrams.Prelude
import Diagrams.Backend.SVG
import XenFret.Util
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import qualified Data.List.NonEmpty as NonEmpty
import XenFret.Data

-- | Generate an infinite list of the notes of the scale, from 0.
repeatingNotes :: Scale -> [Int]
repeatingNotes (Scale _ xs) = scanl (+) 0 (join $ repeat $ toList xs)

data Str = Str {
    -- | Non-negative integer,
    --    describes the relative tuning of strings 
    pitch :: Int,        
    -- | Non-negative integers, a list of marked scale
    --   positions on the string.
    notes :: [Int]  
}

-- | Make a string with no notes
mkStr n = Str { pitch = n, notes = [] }

data Fretboard = Fretboard {
    -- | Non-empty list of strings in a fretboard 
    fretboardStrings :: NonEmpty Str,
    -- | The number of steps to a period in a fretboard.
    fretboardPeriod :: Int 
}

-- | Validates the preconditions of and creates a fretboard.
makeFret :: NonEmpty Int -- Tuning of the fretboard, must be non-empty, non-zero.
         -> Int  -- Period, must be a positive number.
         -> Either [String] Fretboard
makeFret tuning period 
    | period >= 1 && not (null tuning) && not (any (< 0) tuning)
        = Right $ Fretboard (NonEmpty.map mkStr tuning) period
    | otherwise
        = Left $ collectErrList [
            (null tuning     , "Tuning is empty -- no strings specified"),
            (period < 1      , "Period must be a positive number"),
            (any (<0) tuning , "No strings can have a non-positive tuning")]

-- | Change the scale of a fretboard
changeScale :: Fretboard -> Scale -> Fretboard
changeScale f@(Fretboard strs period) s@(Scale _ intervals) = 
    Fretboard
        (go (fretboardStrings $ applyFirst f s) 1) 
        period
  where 
    go :: NonEmpty Str -> Int -> NonEmpty Str
    go fb n 
        | n < length fb
            = go (notes str1 # map (\x -> x - pitch (toList fb !! n)) 
                    # filterOutInc (<0) 
                    # (\x -> fb & set (element n) Str{ notes = x, pitch = pitch (toList fb !! n)})) 
                 (n + 1)
        | otherwise = fb
      where str1 = NonEmpty.head fb

-- | Applies the scale to the first string
applyFirst :: Fretboard -> Scale -> Fretboard
applyFirst (Fretboard (s :| ss) period) (Scale name intervals)
    | period == sum intervals
        = Fretboard (Str { notes = repeatingNotes (Scale name intervals), pitch = pitch s } :| ss) period
    | otherwise = error "Periods do not match"

-- | Convert a list of positions to a diagram of the dots at those positions (with a given
-- vertical and horizontal spacing)
frettingDots :: Double       -- Vertical spacing
          -> Double       -- Horizontal spacing
          -> [Int]        -- List of fret locations, all Ints should be non-zero.
          -> Diagram B -- A diagram of the dots.
frettingDots vs hs locs = foldr1 atop $ map (`frettingDot` vs) locs

-- | Create a diagram of a single dot
frettingDot :: Int -> Double -> Diagram B
frettingDot 0 vs = circle 0.03 # lwL 0.007
frettingDot n vs = circle (0.03 * 0.8) # fc black # lwL 0.007 # translateY (-n'*vs)
        where n'  = fromIntegral n  :: Double

-- | Create a fretboard diagram
board ::    Int    -- Number of frets to display on board.
           -> Double -- Vertical spacing
           -> Double -- Horizontal spacing
           -> Fretboard
           -> Diagram B
board nFrets vs hs fretboard = frame 0.005 $
    emptyboard
       `atop`
    -- The dots, translated to their proper positions on the fretboard
    foldl1 atop (zipWith translateX (map (* hs) [0..nStr'])
                    (map (translateX (-0.5 * (nStr' - 1) * hs)) dots))
  where 
    emptyboard = emptyBoard nFrets vs hs nStr
    nStr       = length $ fretboardStrings fretboard
    dots       = map (frettingDots vs hs) positions
    positions  = map (takeWhile (<= nFrets) . notes) (toList $ fretboardStrings fretboard)
    nStr'      = fromIntegral nStr :: Double -- Type cast


-- | An empty fretboard diagram.
emptyBoard :: Int    -- Number of frets to display on board.
           -> Double -- Vertical spacing
           -> Double -- Horizontal spacing
           -> Int    -- Number of strings
           -> Diagram B
emptyBoard nFrets vs hs nStr =
     -- Nut
    hrule width
        `atop` markers 
        -- The strings translated to their proper poisitons
        `atop` strings 
            # translateX (-width/2)
            # translateY (-len/2)
    -- The fretboard extends out 1/2 of a vs past the last fret, hence:
  where
    len      = (nFrets' + 1/2) * vs
    width    = (nStr' - 1) * hs
    markers  = case () of
        () | nStr > 1 -> vcat' 
                (with & sep .~ vs) 
                (replicate nFrets (hrule width)
                    # dashingL [0.03] 0
                    # lwL 0.007)
                  # translateY (-vs)
           -- Make the frets more visible when there is only one string.
           | nStr == 1 -> vcat' 
                (with & sep .~ vs) 
                (replicate nFrets (hrule hs)
                    # lwL 0.007)
                  # translateY (-vs)
           | otherwise -> 
               error "Must have at least one string."
    strings = hcat' (with & sep .~ hs) $
        replicate nStr (vrule len)

    -- Type casts
    nFrets' = fromIntegral nFrets :: Double
    nStr'   = fromIntegral nStr :: Double

