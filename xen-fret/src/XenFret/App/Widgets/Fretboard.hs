
module XenFret.App.Widgets.Fretboard where

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Maybe
import Graphics.Svg

import XenFret
import XenFret.Util
import XenFret.Data
import XenFret.AppData
import XenFret.App.Util

import Diagrams.Prelude ( renderDia, mkWidth, Diagram )
import Diagrams.Backend.SVG

data XorY = X Int | Y Int

-- | Generate the formatted SVG output from a diagram.
format :: XorY -> Diagram B -> String
format (X x) d = T.unpack $ decodeUtf8 $ B.toStrict $
    renderBS $ renderDia SVG (SVGOptions (mkWidth (fromIntegral x)) Nothing "" [] False) d
format (Y y) d = T.unpack $ decodeUtf8 $ B.toStrict $
    renderBS $ renderDia SVG (SVGOptions (mkWidth (fromIntegral y)) Nothing "" [] False) d

-- | Handle the error messages from parsing the arguments.
handleParseErrs :: Maybe Int
    -> Maybe Int
    -> Maybe [Int]
    -> Maybe Int
    -> Maybe Int
    -> Maybe Int
    -> Either String (Int, Int, NonEmpty Int, Int, XorY)
handleParseErrs period frets tuning skipFrets x y
  -- Valid format
  | Just period' <- period
  , Just frets' <- frets
  , Just tuning' <- NE.nonEmpty =<< tuning
  , isJust x `xor` isJust y
  , Just skipFrets' <- skipFrets
  = case (x,y) of
       (Just x',_) -> Right (period',frets',tuning', skipFrets', X x')
       (_,Just y') -> Right (period',frets',tuning', skipFrets', Y y')
       _           -> Left "Must have either x or y specified."
  -- Errors, invalid format
  |  otherwise
  = collectErrors [
      (isNothing period, "Error parsing period, should be an integer"),
      (isNothing frets, "Error parsing frets, should be an integer"),
      (isNothing tuning, "Error parsing tuning, should be a list of integers, (e.a. [1,2,3])"),
      (isNothing x && isNothing y, "Neither given an x, nor a y"),
      (isJust x && isJust y, "Given both an x and a y, only one or the other can be given")]

-- | Handle the error messages from constructing the scales and fretboard
handleScaleFretboardErrs :: Either [String] Fretboard -> [Either [String] Scale] -> Either [String] (Fretboard,[Scale])
handleScaleFretboardErrs fb scls = do
    fb' <- fb
    scls' <- sequence scls
    pure (fb', scls')

-- | Include which no. scale the errors are coming from.
indexErrs :: [Either [String] a] -> [String]
indexErrs xs = go 1 xs []
    where go n [] idx = idx
          go n ((Left errs):xs) idx = go (n+1) xs (idx++["For scale #" ++ show n ++ ": " ++ fmt errs])
          go n ((Right _):xs)   idx = go (n+1) xs idx

          fmt errs = foldl1 (\x y -> x++", "++y) errs
