
module XenFret (
    makeFret,
    changeScale,
    repeatingNotes,
    board,
    transpose,
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
repeatingNotes (Scale _ xs) =
  scanl (+)
       -- Start one period down to account for
       -- transpositions.
       (-period)
       (join $ repeat $ toList xs)
  where
    period = sum xs

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

transposeStr n (Str pitch notes) = Str pitch (fmap (+ 1) notes)

data Fretboard = Fretboard {
    -- | Non-empty list of strings in a fretboard 
    fretboardStrings :: NonEmpty Str,
    -- | The number of steps to a period in a fretboard.
    fretboardPeriod :: Int
}

transpose :: Int -> Fretboard -> Fretboard
transpose n (Fretboard strings period) = Fretboard
    (fmap (transposeStr n) strings)
    period

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
changeScale :: Fretboard -> Int -> Scale -> Fretboard
changeScale f@(Fretboard strs period) key s@(Scale _ intervals) =
    Fretboard
        (go (fretboardStrings $ applyFirst f key s) key 1)
        period
  where
    go :: NonEmpty Str -> Int -> Int -> NonEmpty Str
    go fb key n
        | n < length fb
            = go (notes str1 # map (\x -> x - pitch (toList fb !! n))
                    # filterOutInc (< 0)
                    # (\x -> fb & set (element n) Str { notes = x, pitch = pitch (toList fb !! n)}))
                 key
                 (n + 1)
        | otherwise = fb
      where str1 = NonEmpty.head fb

-- | Applies the scale to the first string
applyFirst :: Fretboard -> Int -> Scale -> Fretboard
applyFirst (Fretboard (s :| ss) period) key (Scale name intervals)
    | period == sum intervals
        = Fretboard (Str { notes = filterOutInc (< 0) $ (+ (key `mod` period)) <$> repeatingNotes (Scale name intervals), pitch = pitch s } :| ss) period
    | otherwise = error "Periods do not match"

-- | Convert a list of positions to a diagram of the dots at those positions (with a given
-- vertical and horizontal spacing)
frettingDots :: Int
  -> Double    -- Vertical spacing
  -> Double    -- Horizontal spacing
  -> [Int]     -- List of fret locations, all Ints should be non-zero.
  -> Diagram B -- A diagram of the dots.
frettingDots offset vs hs =
    foldr (atop . frettingDot offset vs) mempty
  . fmap ((-offset) +)
  . filterOutInc (< offset)

-- | Create a diagram of a single dot
frettingDot :: Int -> Double -> Int -> Diagram B
frettingDot 0 vs 0 = circle 0.03 # lwL 0.007
frettingDot _ vs n = circle (0.03 * 0.8) # fc black # lwL 0.007 # translateY (-n'*vs)
        where n'  = fromIntegral n  :: Double

-- | Create a fretboard diagram
board :: String
  -> Int
  -> Int    -- Number of frets to display on board.
  -> Double -- Vertical spacing
  -> Double -- Horizontal spacing
  -> Fretboard
  -> Maybe [String]
  -> Diagram B
board scaleName offset nFrets vs hs fretboard optNoteNames = frame 0.005 $
        ((baselineText scaleName # scale 0.075) <> strutY 0.12 <> strutX 1.0)
            === (
            noteMarkers |||
                (emptyboard
                    `atop`
                    -- The dots, translated to their proper positions on the fretboard
                 foldl1 atop (zipWith translateX (map (* hs) [0..nStr'])
                    (map (translateX (-0.5 * (nStr' - 1) * hs)) dots)))
        )

  where
    emptyboard = emptyBoard nFrets vs hs nStr
    nStr       = length $ fretboardStrings fretboard
    dots       = map (frettingDots offset vs hs) positions
    positions  = map (takeWhile (<= (nFrets + offset)) . notes) (toList $ fretboardStrings fretboard)
    nStr'      = fromIntegral nStr :: Double -- Type cast
    noteMarkers = case optNoteNames of
        Nothing -> vcat'
            (with & sep .~ vs)
            (replicate nFrets $ strutX 0.1)
        Just noteNames -> let ?noteNames = optNoteNames in
            let offsetNoteNames = fmap displayNote [offset..offset + nFrets] in
                vcat'
                    (with & sep .~ vs)
                    (take (nFrets + 1) $
                        offsetNoteNames <&> \note ->
                            text note # bold # scale 0.037
                                 <> strutX 0.15)


-- | An empty fretboard diagram.
emptyBoard :: Int    -- Number of frets to display on board.
  -> Double -- Vertical spacing
  -> Double -- Horizontal spacing
  -> Int    -- Number of strings
  -> Diagram B
emptyBoard nFrets vs hs nStr =
      nut
        `atop` fretMarkers
        -- The strings translated to their proper poisitons
        `atop` strings
            # translateX (-width/2)
            # translateY (-len/2)
    -- The fretboard extends out 1/2 of a vs past the last fret, hence:
  where
    nut      = hrule width
    len      = (nFrets' + 1/2) * vs
    width    = (nStr' - 1) * hs
    fretMarkers  = case () of
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

