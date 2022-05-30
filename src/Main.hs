module Main where

import Network.CGI
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

-- Constants for now, TODO: allow manual control from the CGI interface.
vs = 0.2
hs = 0.5/5

data XorY = X Int | Y Int

-- | CGI entry-point:
-- Read in the CGI arguments, then either output an appropriate
-- error message, or output the svg of the fretboard scale
-- diagram requested.
-- TODO: Maybe use the error monad transformer here.
cgiMain :: CGI CGIResult
cgiMain = do
    p <- readInput "period" :: CGI (Maybe Int)
    f <- readInput "frets" :: CGI (Maybe Int)
    s <- readInput "scales" :: CGI (Maybe [[Int]])
    t <- readInput "tuning" :: CGI (Maybe [Int])
    x <- readInput "x" :: CGI (Maybe Int)
    y <- readInput "y" :: CGI (Maybe Int)

    -- Handle errors parsing the cgi arguments
    case handleCGIParseErrs p f s t x y of
        Left err                              -> output err
        Right (period, frets, scales, tuning, xy) -> do
            let _fretboard = mkFret tuning period
            let _scales    = map (mkScl period) scales
            case handleScaleFretboardErrs _fretboard _scales of
                Left err                 -> output $ concatErrors err
                Right (fretboard,scales) -> do
                    let diagrams = map (toBoard frets vs hs . chScale fretboard) scales
                    case xy of
                        X x -> output $ foldl1 (\x y -> x++"\n &nbsp;&nbsp;&nbsp; \n"++y) $ map (format (X x)) diagrams
                        Y y -> output $ foldl1 (\x y -> x++"\n &nbsp;&nbsp;&nbsp; \n"++y) $ map (format (Y y)) diagrams

-- | Generate the formatted SVG output from a diagram.
format :: XorY -> Diagram B -> String
format (X x) d = B.unpack $ renderBS $ renderDia SVG (SVGOptions (mkWidth (fromIntegral x)) Nothing "" [] False) d
format (Y y) d = B.unpack $ renderBS $ renderDia SVG (SVGOptions (mkWidth (fromIntegral y)) Nothing "" [] False) d

-- | Handle the error messages from parsing the arguments.
handleCGIParseErrs :: Maybe Int -> Maybe Int -> Maybe [[Int]]
       -> Maybe [Int] -> Maybe Int -> Maybe Int
       -> Either String (Int, Int, [NonEmpty Int], NonEmpty Int, XorY)
handleCGIParseErrs p f s t x y
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
main = runCGI (handleErrors cgiMain)
