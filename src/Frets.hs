module Frets (
    mkScl,
    mkFret,
    chScale,
    infScl,
    toBoard,
    Fretboard(),
    Scale(), 
) where

import Diagrams.Attributes
import Control.Monad
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Control.Monad.Loops
import Control.Lens hiding ((#))
import Control.Monad.State
import Frets.Util

-- TODO: Clean up newtype code
newtype Scale = Scale ([Int], -- Intervals of the scale, must not contain 0,
			           Int)   -- Period of the scale, the intervals must
                              -- sum to this.
fromScl (Scale x) = x

-- | Validates the preconditions of, and creates a scale.
mkScl :: Int -> [Int] -> Either [String] Scale
mkScl n xs | not $ any (<=0) xs
		   , sum xs == n 
           = Right (Scale (xs,n))
		   | otherwise 
           = Left $ collectErrList [(sum xs /= n  , "The sum of the intervals is not equal to the period"),
                                    (any (<=0) xs , "Non-positive intervals are not allowed")]
           
-- | Generate an infinite list of the notes of the scale, from 0.
infScl :: Scale -> [Int]
infScl (Scale (xs,_)) = scanl (+) 0 (join $ repeat xs)

-- Only used internally
-- TODO: This might be more readable as a regular constructor
data Str = Str {
	pitch :: Int,   -- Non-negative integer,
    			    -- describes the relative tuning of strings      
    notes :: [Int]  -- Non-negative integers, a list of marked scale
    				-- positions on the string.
}

-- | Make a string with no notes
mkStr n = Str { pitch = n, notes = [] }

newtype Fretboard = Fretboard ([Str],
				                Int) -- The number of steps to a period in a fretboard.
fromFret (Fretboard x) = x

-- | Validates the preconditions of and creates a fretboard.
mkFret ::   [Int] -- Tuning of the fretboard, must be non-empty, non-zero.
		  -> Int  -- Period, must be a positive number.
          -> Either [String] Fretboard
mkFret t period | period >= 1 && (length t) >= 1 && (not $ any (<0) t)
                = Right $ Fretboard (map mkStr t, period)
                | otherwise
                = Left $ collectErrList [((length t) < 1, "Tuning is empty -- no strings specified"),
                                          (period < 1   , "Period must be a positive number"),
                                          (any (<0) t   , "No strings can have a non-positive tuning")]

-- | Change the scale of a fretboard
chScale :: Fretboard -> Scale -> Fretboard
chScale f@(Fretboard (strs,p)) s@(Scale scl) = Fretboard (go (fst $ fromFret $ applyFirst f s) 1,p)
    where go :: [Str] -> Int -> [Str]
          go fb n | n < (length fb)
                   = go (notes str1 # map (\x -> x-(pitch $ fb !! n)) #
                         filterOutInc (<0) #
                         (\x -> fb & set (element n) Str{notes=x, pitch=(pitch (fb !! n))})) (n+1)
                   | otherwise = fb
              where str1 = fb !! 0

-- | Applies the scale to the first string
applyFirst :: Fretboard -> Scale -> Fretboard
applyFirst (Fretboard (s:ss,p)) (Scale scl) 
    | p == (snd scl)
	= Fretboard (Str{notes=(infScl (Scale scl)),pitch=(pitch s)}:ss,p)
    | otherwise = error "Periods do not match"

-- | Convert a list of positions to a diagram of the dots at those positions (with a given
-- vertical and horizontal spacing)
toDots    :: Double       -- Vertical spacing
		  -> Double       -- Horizontal spacing
          -> [Int]        -- List of fret locations, all Ints should be non-zero.
          -> Diagram B R2 -- A diagram of the dots.
toDots vs hs locs = foldr1 atop $ map (\x -> toDot x vs) locs

-- | Create a diagram of a single dot
toDot :: Int -> Double -> Diagram B R2
toDot 0 vs = circle 0.03 # lwL 0.007
toDot n vs = circle (0.03*0.8) # fc black # lwL 0.007 # translateY (-n'*vs)
	where n'  = fromIntegral n  :: Double

-- | Create a fretboard diagram
toBoard ::    Int    -- Number of frets to display on board.
		   -> Double -- Vertical spacing
           -> Double -- Horizontal spacing
		   -> Fretboard 
           -> Diagram B R2
toBoard n_frets vs hs fretboard = 
  emptyboard 
   `atop` 
  -- The dots, translated to their proper positions on the fretboard
  (foldl1 atop $ zipWith (\x y -> translateX x y) (map (*hs) [0..n_str'])
                         (map (translateX (-1/2*(n_str'-1)*hs)) dots))
    where emptyboard = emptyBoard n_frets vs hs n_str
          n_str      = length $ fst $ fromFret fretboard
          n_str'     = fromIntegral n_str
          dots       = map (toDots vs hs) positions
          positions  = map ((takeWhile (<=n_frets)) . notes) (fst $ fromFret fretboard)


-- | Draws an empty fretboard diagram.
emptyBoard :: Int    -- Number of frets to display on board.
		   -> Double -- Vertical spacing
           -> Double -- Horizontal spacing
           -> Int    -- Number of strings
           -> Diagram B R2
emptyBoard n_frets vs hs n_str = 
    hrule width -- Nut
    `atop` markers
    `atop` strings # translateX (-width/2)
    			   # translateY (-len/2)

    where len      = (n_frets'+1/2)*vs
    	  width    = (n_str'-1)*hs
          markers  = case () of
                      () | n_str > 1 -> (vcat' (with & sep .~ vs) $
          	                             replicate n_frets (hrule width) 
                                       # dashingL [0.03] 0 
                                       # lwL 0.007)
                                       # translateY (-vs)
                         | n_str == 1 -> (vcat' (with & sep .~ vs) $
          	                             replicate n_frets (hrule hs) 
                                       # lwL 0.007)
                                       # translateY (-vs)
          strings = hcat' (with & sep .~ hs) $
          					replicate n_str (vrule len)
          
          -- Type casts
          n_frets' = fromIntegral n_frets :: Double
          n_str'   = fromIntegral n_str :: Double

