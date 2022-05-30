module Main where

import Data.Maybe (isJust, isNothing)
import Control.Monad
import Data.Either
import Diagrams.Prelude ( renderDia, mkWidth, Diagram )
import Diagrams.Backend.SVG
import qualified Data.ByteString.Lazy.Char8 as B
import Frets.Util
import Frets
import Graphics.Svg
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Reflex.Dom.Core hiding(Home, button)
import Reflex.Dom.Old (elDynHtml')
import Language.Javascript.JSaddle.Warp
import qualified Data.Text as T
import Utils hiding(Scale)
import Data.Functor
import Control.Applicative
import qualified Data.Map as Map
import qualified Data.List.NonEmpty as NE

-- Constants for now, TODO: allow manual control from the CGI interface.
vs :: Double
vs = 0.2

hs :: Double
hs = 0.5/5

data XorY = X Int | Y Int

header :: _ => m ()
header = do
  el "title" $ text "Xen Fret"
  elAttr "meta" (
    "name" =: "viewport" <>
    "content" =: "width=device-width, initial-scale=1") blank
  elAttr "script" (
    "src" =: "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/js/materialize.min.js") blank
  elAttr "link" (
    "id" =: "css-style" <>
    "href" =: "https://sintrastes.github.io/demos/montague/materialize.min.css" <>
    "type" =: "text/css" <>
    "rel" =: "stylesheet") blank
  elAttr "link" (
    "href" =: "https://sintrastes.github.io/demos/montague/main.css" <>
    "type" =: "text/css" <>
    "rel" =: "stylesheet") blank
  elAttr "link" (
    "href" =: "https://fonts.googleapis.com/icon?family=Material+Icons" <>
    "type" =: "text/css" <>
    "rel" =: "stylesheet") blank
  elAttr "link" (
    "href" =: "https://sintrastes.github.io/demos/montague/w3.css" <>
    "type" =: "text/css" <>
    "rel" =: "stylesheet") blank
  elAttr "link" (
    "id" =: "material-colors" <>
    "href" =: "https://sintrastes.github.io/demos/montague/material-colors-default.css" <>
    "type" =: "text/css" <>
    "rel" =: "stylesheet") blank

data Pages = 
    Home 
  | Preferences 
  | Temperaments
  | Tunings
  | Scales
  | EditScale
  | EditTemperament
    deriving(Show)

app :: _ => m ()
app = do
    let appData = defaultAppData

    let loadedTemperaments = temperaments appData
    let Just loadedScales = Map.lookup "12-TET" $ scales appData

    materialNavBar [Home, Temperaments, Tunings, Scales, Preferences]

    el "p" $ text "Configuration options:"

    temperament <- selectMaterial "Temperament" loadedTemperaments 
        (head loadedTemperaments)

    f <- labeledEntry "Frets" intEntry 10
    s <- selectMaterial "Scale" loadedScales (head loadedScales)
    t <- pure $ Just [0,5,10] -- readInput "tuning" :: CGI (Maybe [Int])
    x <- labeledEntry "Width" intEntry 82

    el "p" $ text "Fretboard preview: "
    button "Save"

    -- Handle errors parsing the arguments
    dyn $ (liftA2 (,) (liftA2 (,) f x) (liftA2 (,) s temperament)) <&> \((frets, xSize), (scale, temperament')) -> case handleParseErrs (Just $ period $ temperament') (Just frets) (Just $ [NE.toList $ scaleNotes scale]) t (Just xSize) Nothing of
        Left err                              -> el "p" $ text $ T.pack err
        Right (period, frets, scales, tuning, xy) -> do
            let _fretboard = mkFret tuning period
            let _scales    = map (mkScl period) scales
            case handleScaleFretboardErrs _fretboard _scales of
                Left err                 -> el "p" $ text $ T.pack $ concatErrors err
                Right (fretboard,scales) -> elAttr "div" ("class" =: "main-column" <> "style" =: "text-align: center;") $ do
                    let diagrams = map (toBoard frets vs hs . chScale fretboard) scales
                    case xy of
                        X x -> do
                            elDynHtml' "div" (constDyn $ T.pack $
                                foldl1 (\x y -> x++"\n &nbsp;&nbsp;&nbsp; \n"++y) $ map (format (X x)) diagrams)
                            pure ()
                        Y y -> do
                            elDynHtml' "div" (constDyn $ T.pack $
                                foldl1 (\x y -> x++"\n &nbsp;&nbsp;&nbsp; \n"++y) $ map (format (Y y)) diagrams)
                            pure ()
    blank

-- | Generate the formatted SVG output from a diagram.
format :: XorY -> Diagram B -> String
format (X x) d = B.unpack $ renderBS $ renderDia SVG (SVGOptions (mkWidth (fromIntegral x)) Nothing "" [] False) d
format (Y y) d = B.unpack $ renderBS $ renderDia SVG (SVGOptions (mkWidth (fromIntegral y)) Nothing "" [] False) d

-- | Handle the error messages from parsing the arguments.
handleParseErrs :: Maybe Int -> Maybe Int -> Maybe [[Int]]
       -> Maybe [Int] -> Maybe Int -> Maybe Int
       -> Either String (Int, Int, [NonEmpty Int], NonEmpty Int, XorY)
handleParseErrs p f s t x y
  -- Valid format
  | Just p' <- p
  , Just f' <- f
  , Just s' <- mapM nonEmpty =<< s
  , Just t' <- nonEmpty =<< t
  , isJust x `xor` isJust y
  = case (x,y) of
       (Just x',_) -> Right (p',f',s',t',X x')
       (_,Just y') -> Right (p',f',s',t',Y y')
       _           -> Left "Must have either x or y specified."
  -- Errors, invalid format
  |  otherwise
  = collectErrors [(isNothing p, "Error parsing period, should be an integer"),
                   (isNothing f, "Error parsing frets, should be an integer"),
                   (isNothing s, "Error parsing scales, should be a list of a list of integers (e.a. [[1,2,3],[4,5,6]])"),
                   (isNothing t, "Error parsing tuning, should be a list of integers, (e.a. [1,2,3])"),
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

main :: IO ()
main = run 3911 $ mainWidgetWithHead header app
