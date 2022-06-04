
module Frets (
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
import Frets.Util
import Utils
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import qualified Data.List.NonEmpty as NonEmpty

-- | Generate an infinite list of the notes of the scale, from 0.
repeatingNotes :: Scale -> [Int]
repeatingNotes (Scale _ xs) = scanl (+) 0 (join $ repeat $ toList xs)

data Str = Str {
    pitch :: Int,   -- Non-negative integer,
                    -- describes the relative tuning of strings      

    notes :: [Int]  -- Non-negative integers, a list of marked scale
                    -- positions on the string.
}

-- | Make a string with no notes
mkStr n = Str { pitch = n, notes = [] }

newtype Fretboard = Fretboard (NonEmpty Str,
                               Int) -- The number of steps to a period in a fretboard.
fromFret (Fretboard x) = x

-- | Validates the preconditions of and creates a fretboard.
makeFret :: NonEmpty Int -- Tuning of the fretboard, must be non-empty, non-zero.
         -> Int  -- Period, must be a positive number.
         -> Either [String] Fretboard
makeFret t period 
    | period >= 1 && not (null t) && not (any (< 0) t)
        = Right $ Fretboard (NonEmpty.map mkStr t, period)
    | otherwise
        = Left $ collectErrList [
            (null t, "Tuning is empty -- no strings specified"),
            (period < 1   , "Period must be a positive number"),
            (any (<0) t   , "No strings can have a non-positive tuning")]

-- | Change the scale of a fretboard
changeScale :: Fretboard -> Scale -> Fretboard
changeScale f@(Fretboard (strs,p)) s@(Scale _ scl) = Fretboard (go (fst $ fromFret $ applyFirst f s) 1, p)
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
applyFirst (Fretboard (s :| ss, p)) (Scale name scl)
    | p == sum scl
        = Fretboard (Str { notes = repeatingNotes (Scale name scl), pitch = pitch s } :| ss, p)
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
    nStr       = length $ fst $ fromFret fretboard
    dots       = map (frettingDots vs hs) positions
    positions  = map (takeWhile (<= nFrets) . notes) (toList $ fst $ fromFret fretboard)
    nStr'      = fromIntegral nStr :: Double -- Type cast


-- | Draws an empty fretboard diagram.
emptyBoard :: Int    -- Number of frets to display on board.
           -> Double -- Vertical spacing
           -> Double -- Horizontal spacing
           -> Int    -- Number of strings
           -> Diagram B
emptyBoard nFrets vs hs nStr =
    hrule width -- Nut
    `atop` markers --  Translate the strings to the proper poisitons
    `atop` strings # translateX (-width/2)
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

