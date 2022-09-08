{-# OPTIONS_GHC -Wno-name-shadowing #-}

module XenFret (
    repeatingNotes,
    board,
    getNotes,
    Fretboard(..),
    FretboardStyle(..)
) where

import Diagrams.Attributes
import Control.Monad
import Diagrams.Prelude
import Diagrams.Backend.SVG
import XenFret.Util
import Data.List.NonEmpty (toList)
import XenFret.Data ( displayNote, Scale(Scale, scaleIntervals) )
import Data.Ratio
import Data.Maybe
import XenFret.AppData (PreferenceData (noteNameSize))

-- | Generate an infinite list of the notes of the scale, from 0.
repeatingNotes :: Scale -> [Int]
repeatingNotes (Scale _ xs) =
  scanl (+)
       -- Start one period down to account for
       -- transpositions.
       (-periodSize)
       (join $ repeat $ toList xs)
  where
    periodSize = sum xs

newtype Fretboard = Fretboard {
    fretboardStrings :: [Int]
}

-- | Given a scale, a fretboard, and a skip fretting value,
-- return a list of the note positions on each of the strings
-- of the passed fretboard.
getNotes :: Scale -> Fretboard -> Int -> Int -> [[Int]]
getNotes scale (Fretboard stringTunings) key skipFretting = stringTunings <&> \stringPitch ->
    mapMaybe fromRatio
        $ filterOutInc (< 0)
        $ fmap (\x -> x - (stringPitch % n)) 
        $ fmap (\x -> x + (key % n))
        $ fmap (% n) 
        $ repeatingNotes scale
    where 
      n = skipFretting + 1

fromRatio :: Ratio Int -> Maybe Int
fromRatio x
    | denominator x == 1 = Just $ numerator x
    | otherwise = Nothing

-- | Convert a list of positions to a diagram of the dots at those positions (with a given
-- vertical and horizontal spacing)
frettingDots :: Bool
  -> Int
  -> Double    -- Vertical spacing
  -> Double    -- Horizontal spacing
  -> [(Int, Bool)]     -- List of fret locations, all Ints should be non-zero.
  -> Diagram B -- A diagram of the dots.
frettingDots displayMarkersOnFrets offset vs _ =
    foldr (atop . frettingDot displayMarkersOnFrets offset vs) mempty
  . fmap (\(x,y) -> (x - offset, y))
  . filterOutInc (\(x, _) -> x < offset)

-- | Create a diagram of a single dot
frettingDot :: Bool -> Int -> Double -> (Int, Bool) -> Diagram B
frettingDot _ 0 _ (0, _) = circle 0.03 # lwL 0.007
frettingDot displayMarkersOnFrets _ vs (n, colored) =
    translateY offset $ circle (0.03 * 0.8)
        # fc color
        # lwL 0.007
        # translateY (-n'*vs)
  where
    n'     = fromIntegral n  :: Double
    color  = if colored then blue else black
    offset = if displayMarkersOnFrets
        then 0.0
        else 0.5 * vs

data FretboardStyle = FretboardStyle {
    displayMarkersOnFrets :: Bool,
    offset :: Int,
    nFrets :: Int,
    verticalSpacing :: Double,
    horizontalSpacing :: Double
}

-- | Create a fretboard diagram
board ::
     PreferenceData
  -> String
  -> Int
  -> Scale
  -> Int
  -> Fretboard
  -> Maybe [String]
  -> FretboardStyle
  -> Diagram B
board prefs scaleName key scale' skipFrets fretboard optNoteNames FretboardStyle{..} = frame 0.005 $
        (alignL (baselineText scaleLabelFormatted # scale 0.075) <> strutY 0.12)
            ===
            (translateY (-0.12) (alignT noteMarkers) |||
                (alignL stringMarkers === alignL (
                    emptyboard
                        `atop`
                        -- The dots, translated to their proper positions on the fretboard
                       let translatedDots = zipWith translateX (map (* hs) [0..nStr'])
                            (map (translateX (-0.5 * (nStr' - 1) * hs)) dots)
                        in foldl1 atop
                            translatedDots)
                )
        )

  where
    vs = verticalSpacing
    hs = horizontalSpacing
    emptyboard = emptyBoard nFrets vs hs nStr offset
    scaleLabelFormatted =  case optNoteNames of
        Nothing        -> scaleName
        Just _ -> let ?noteNames = optNoteNames in
            displayNote key ++ " " ++ scaleName
    strings = fretboardStrings fretboard
    lowestNote = head strings
    notes    = getNotes scale' fretboard key skipFrets
    
    dots       = fmap (frettingDots displayMarkersOnFrets offset vs hs) positions
    positions  = (\(xs, p) -> fmap (markRoot p) xs) <$> zip unmarkedPositions strings
    unmarkedPositions = fmap (takeWhile (<= (nFrets + offset)))
            notes
    
    nStr       = length strings
    nStr'      = fromIntegral nStr :: Double -- Type cast

    -- Note: This could probably be generalized to find the degree of
    --  a note as well.
    -- Note: This will need to be fixed to account for
    --  skip fretting.
    markRoot :: Int -> Int -> (Int, Bool)
    markRoot stringPitch x
        | (stringPitch + x) `mod` scalePeriod == key `mod` scalePeriod
            = (x, True)
        | otherwise
            = (x, False)

    scalePeriod = sum (scaleIntervals scale')

    stringMarkers :: Diagram B
    stringMarkers =
        if offset /= 0
            then mempty
            else case optNoteNames of
                Nothing -> hcat'
                    (with & sep .~ hs)
                    (replicate (length stringPitches) $ strutY 0.1)
                Just _ -> let ?noteNames = optNoteNames in
                    let stringNoteNames = fmap displayNote stringPitches in
                        hcat'
                            (with & sep .~ hs)
                            (stringNoteNames <&> \note ->
                                    text note # bold # scale (baseFontSize * noteFontSize)
                                        <> strutY 0.15)
    noteMarkers :: Diagram B
    noteMarkers = offsetMarkers $ case optNoteNames of
        Nothing -> vcat'
            (with & sep .~ vs)
            (replicate nFrets $ strutX 0.1)
        Just _ -> let ?noteNames = optNoteNames in
            let offsetNoteNames = fmap displayNote
                        [(offset + lowestNote + 1)..(offset + lowestNote) + nFrets] in
                vcat'
                    (with & sep .~ vs)
                    (take (nFrets + 1) $
                        offsetNoteNames <&> \note ->
                            text note # bold # scale (baseFontSize * noteFontSize)
                                 <> strutX 0.15)

    noteFontSize = fromIntegral $ noteNameSize prefs

    offsetMarkers
      | offset == 0 =
        if displayMarkersOnFrets
            then (strutY vs ===)
            else (strutY (0.5 * vs) ===)
      | displayMarkersOnFrets = (strutY (0.5 * vs) ===)
      | otherwise = id

    stringPitches = fretboardStrings fretboard

baseFontSize = 0.0033

-- | An empty fretboard diagram.
emptyBoard :: Int    -- Number of frets to display on board.
  -> Double -- Vertical spacing
  -> Double -- Horizontal spacing
  -> Int    -- Number of strings
  -> Int    -- Offset
  -> Diagram B
emptyBoard nFrets vs hs nStr offset =
      nut
        `atop` fretMarkers
        -- The strings translated to their proper poisitons
        `atop` strings
            # translateX (-width/2)
            # translateY (-len/2)
    -- The fretboard extends out 1/2 of a vs past the last fret, hence:
  where
    nut = if offset == 0
       then hrule width # lwL 0.0125
       else hrule width
                # dashingL [0.03] 0
                # lwL 0.007
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
