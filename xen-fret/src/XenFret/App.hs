
module XenFret.App where

import Data.Maybe (isJust, isNothing, fromJust)
import Control.Monad
import Data.Either
import Diagrams.Prelude ( renderDia, mkWidth, Diagram, (===) )
import qualified Diagrams.Prelude as D
import Diagrams.Backend.SVG
import qualified Data.ByteString.Lazy.Char8 as B
import XenFret.Util
import XenFret
import Graphics.Svg
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Reflex.Dom.Core hiding(Home, button, checkbox)
import Reflex.Dom.Old (elDynHtml')
import Reflex.Dom.Extras
import qualified Data.Text as T
import Data.Functor
import Control.Applicative
import qualified Data.Map as Map
import qualified Data.List.NonEmpty as NE
import Control.Exception
import System.Info
import System.Directory
import Data.List hiding (transpose)
import Data.Aeson
import Control.Monad.IO.Class
import GHC.Float
import Language.Javascript.JSaddle (eval, liftJSM, jsg3)
import XenFret.Data
import XenFret.AppData

baseVerticalSpacing :: Double
baseVerticalSpacing = 0.2

baseHorizontalSpacing :: Double
baseHorizontalSpacing = 0.5/5

data XorY = X Int | Y Int

header :: _ => m ()
header = do
  el "title" $ text "Xen Fret"
  elAttr "meta" (
    "name" =: "viewport" <>
    "content" =: "width=device-width, initial-scale=1") blank
  elAttr "script" (
    "src" =: "https://sintrastes.github.io/demos/xen_fret/main.js") blank
  elAttr "script" (
    "src" =: "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/js/materialize.min.js") blank
  elAttr "link" (
    "id" =: "css-style" <>
    "href" =: "https://sintrastes.github.io/demos/montague/materialize.min.css" <>
    "type" =: "text/css" <>
    "rel" =: "stylesheet") blank
  elAttr "link" (
    "href" =: "https://sintrastes.github.io/demos/xen_fret/main.css" <>
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

loadAppData dataFile = do
    loadedData :: AppData <- liftFrontend defaultAppData $
        catch (fromJust <$> decodeFileStrict dataFile)
            (\(e :: SomeException) -> return defaultAppData)
    pure loadedData

persistAppData dynAppData dataFile = 
    prerender (pure never) $ performEvent $ updated dynAppData <&> 
        \newData ->
            liftIO $ encodeFile dataFile newData

app :: _ => m ()
app = do
    -- Setup the application directory.
    appDir <- if "android" `isInfixOf` os
        then pure "/data/data/org.xenfret.app"
        else liftFrontend "/" getHomeDirectory <&> (<> "/.xenfret")

    toastOnErrors $ liftFrontend (Right ()) $ catch
        (do createDirectoryIfMissing True appDir
            pure $ Right ())
        (\(e :: SomeException) -> pure $ Left e)
    
    navEvents <- materialNavBar [Home, Temperaments, Tunings, Scales, Preferences]
    currentPage <- holdDyn Home navEvents

    dyn $ currentPage <&> \case
        Home -> mainPage appDir
        Temperaments -> temperamentPage appDir
        Tunings -> tuningPage appDir
        Scales -> scalePage appDir
        Preferences -> preferencePage appDir
    blank

mainPage :: _ => FilePath -> m ()
mainPage appDir = do
    appData <- loadAppData (appDir <> "/app_data.json")
    let loadedTemperaments = temperaments appData

    elAttr "div" ("style" =: "display: flex;height:100%;") $ do
        (saveEvent, dynArgs) <- elClass "div" "main-pane-left" $ do
            elAttr "h5" ("style" =: "padding-bottom: 10px;") $ text "Diagram Options:"

            temperamentDyn <- elClass "div" "row" $
                selectMaterial "Temperament" 
                    "No Temperaments Defined" 
                    (pure loadedTemperaments)
                    (head loadedTemperaments)

            let Just initialTunings = Map.lookup "12-TET" $ tunings appData

            let loadedTunings = temperamentDyn <&> (\temperamentMay -> maybe [] id $ do
                    temperament <- temperamentMay
                    Map.lookup (temperamentName temperament) $ tunings appData)

            tuningDyn <- elClass "div" "row" $
                selectMaterial "Instrument/Tuning" 
                    "No Tunings Defined" 
                    loadedTunings
                    (head initialTunings)

            let Just initialScales = Map.lookup "12-TET" $ scales appData

            let loadedScales = temperamentDyn <&> (\temperamentMay -> maybe [] id $ do
                    temperament <- temperamentMay
                    Map.lookup (temperamentName temperament) $ scales appData)

            scaleDyn <- elClass "div" "row" $
                selectMaterial "Scale"
                    "No Scales Defined" 
                    loadedScales 
                    (head initialScales)

            (keyDyn, offsetDyn) <- elAttr "div" ("class" =: "row" <> "style" =: "margin-bottom: 0px;") $ do
                keyDyn <- elAttr "div" ("class" =: "col s6" <> "style" =: "padding-left: 0px;") $ 
                    labeledEntry "Key" positiveIntEntry 0
                offsetDyn <- elAttr "div" ("class" =: "col s6" <> "style" =: "padding-right: 0px;") $ 
                    labeledEntry "Fret Offset" positiveIntEntry 0
                pure (keyDyn, offsetDyn)

            elAttr "h5" ("style" =: "padding-bottom: 10px;") $ text "Display Options:"
            
            (sizeDyn, fretsDyn) <- elClass "div" "row" $ do
                sizeDyn  <- elAttr "div" ("class" =: "col s6" <> "style" =: "padding-left: 0px;") $ 
                    labeledEntry "Size" intEntry 280
                fretsDyn <- elAttr "div" ("class" =: "col s6" <> "style" =: "padding-right: 0px;") $ 
                    labeledEntry "Number of Frets" intEntry 10
                pure (sizeDyn, fretsDyn)

            (verticalScalingDyn, horizontalScalingDyn) <- elAttr "div" ("class" =: "row" <> "style" =: "margin-bottom: 0px;") $ do
                verticalScalingDyn   <- elAttr "div" ("class" =: "col s6" <> "style" =: "padding-left: 0px;") $ 
                    labeledEntry "Vertical Spacing" intEntry 200
                horizontalScalingDyn <- elAttr "div" ("class" =: "col s6" <> "style" =: "padding-right: 0px;") $ 
                    labeledEntry "Horizontal Spacing" intEntry 200
                pure (verticalScalingDyn, horizontalScalingDyn)
            
            elClass "div" "col s12" $ 
                checkbox "Display vertically" True

            elClass "div" "col s12" $ 
                checkbox "Use realistic fret spacing" False
            
            saveEvent <- button "Save"

            pure (saveEvent, (,,,,,,,,) <$>
                fretsDyn <*> 
                sizeDyn <*> 
                scaleDyn <*>
                temperamentDyn <*>
                verticalScalingDyn <*>
                horizontalScalingDyn <*>
                keyDyn <*>
                offsetDyn <*>
                tuningDyn)

        diagramUpdated <- elClass "div" "main-pane-right" $ do
            -- Handle errors parsing the arguments
            dyn $ dynArgs <&> \(frets, xSize, scale, temperament, verticalScaling, horizontalScaling, key, offset, tuning) -> 
                let 
                    verticalSpacing   = (int2Double verticalScaling / 200.0) * baseVerticalSpacing
                    horizontalSpacing = (int2Double horizontalScaling / 200.0) * baseHorizontalSpacing
                in
                    case handleParseErrs (divisions <$> temperament) (Just frets) (NE.toList . stringTunings <$> tuning) (Just xSize) Nothing of
                        Left err -> do
                            el "p" $ text $ T.pack err
                            pure Nothing
                        Right (period, frets, tuning, xy) -> do
                            let _fretboard = makeFret tuning period
                            case handleScaleFretboardErrs _fretboard [maybe (Left ["No scales defined"]) Right scale] of
                                Left err -> do
                                    el "p" $ text $ T.pack $ concatErrors err
                                    pure Nothing
                                Right (fretboard, scales) -> elAttr "div" ("style" =: "text-align: center;") $ do
                                    let diagram = board 
                                            (maybe "" show scale) offset 
                                            frets verticalSpacing horizontalSpacing 
                                                (changeScale fretboard key (fromJust scale))
                                                ((T.unpack <$>) <$> (noteNames =<< temperament))
                                    case xy of
                                        X x -> do
                                            elDynHtml' "div" (constDyn $ T.pack $
                                                format (X x) diagram)
                                            pure $ Just $ format (X x) diagram
                                        Y y -> do
                                            elDynHtml' "div" (constDyn $ T.pack $
                                                format (Y y) diagram)
                                            pure $ Just $ format (Y y) diagram

        diagramDyn <- holdDyn Nothing
            diagramUpdated

        prerender (pure never) $ performEvent $ saveEvent <&> \_ -> do
            maybeSvgText <- sample $ current diagramDyn
            case maybeSvgText of
                Just svgText -> do
                    liftJSM $ jsg3 ("download" :: T.Text) svgText
                        ("diagram.svg" :: T.Text) 
                        ("image/svg" :: T.Text)
                    pure ()
                Nothing -> 
                    toast "Invalid diagram. Cannot save."

        blank    

temperamentPage :: _ => FilePath -> m ()
temperamentPage appDir = do
    appData <- loadAppData (appDir <> "/app_data.json")
    let initialTemperaments = temperaments appData

    newTemperamentEvent <- button "New Temperament"

    newTemperamentSubmitted <- modal newTemperamentEvent $ 
        temperamentForm def

    updatedTemperaments <- switch . current <$> (prerender (pure never) $ performEvent $ newTemperamentSubmitted <&> \case
        Nothing -> pure initialTemperaments
        Just temperament -> do
            toast "Added new temperament" 
            pure (initialTemperaments ++ [temperament]))

    dynTemperaments <- holdDyn initialTemperaments 
        updatedTemperaments

    let dynAppData = dynTemperaments <&> \t ->
            appData { temperaments = t }

    persistAppData dynAppData
        (appDir <> "/app_data.json")

    dyn $ dynTemperaments <&> \currentTemperaments ->
        elClass "ul" "collection" $ do
            forM_ currentTemperaments (\temperament -> do
                elClass "li" "collection-item" $ do
                    el "span" $ text $
                        T.pack $ show temperament)
    blank

temperamentForm :: _ => Temperament -> m (Dynamic t Temperament)
temperamentForm initialValue = do
    modalHeader "Add New Temperament"

    let formContents = Temperament <$>
            form (temperamentName =. labeledEntry "Name" textEntry) <*>
            form (divisions =. labeledEntry "Divisions" intEntry) <*>
            form (period =. labeledEntry "Period" rationalEntry) <*>
            pure Nothing
    
    initForm formContents $ initialValue

tuningPage :: _ => FilePath -> m ()
tuningPage appDir = do
    appData <- loadAppData (appDir <> "/app_data.json")
    let currentTunings = join $ Map.elems $ tunings appData

    newTuningEvent <- button "New Tuning"
    newTuningSubmitted <- modal newTuningEvent $ 
        tuningForm def

    elClass "ul" "collection" $ do
        forM_ currentTunings (\tuning -> do
            elClass "li" "collection-item" $ do
                el "span" $ text $
                    T.pack $ show tuning)

tuningForm :: _ => Tuning -> m (Dynamic t Tuning)
tuningForm initialValue = do
    modalHeader "Add New Tuning"

    labeledEntry "Instrument" textEntry ""
    labeledEntry "Name" textEntry ""
    labeledEntry "Intervals" textEntry ""
    
    pure $ pure initialValue

scalePage :: _ => FilePath -> m ()
scalePage appDir = do
    appData <- loadAppData (appDir <> "/app_data.json")
    let currentScales = join $ Map.elems $ scales appData

    elClass "ul" "collection" $ do
        forM_ currentScales (\scale -> do
            elClass "li" "collection-item" $ do
                el "span" $ text $
                    T.pack $ show scale)

preferencePage :: _ => FilePath -> m ()
preferencePage appDir = do
    appData <- loadAppData (appDir <> "/app_data.json")
    let currentPrefs = preferences appData
    el "p" $ text "Preferences"

-- | Generate the formatted SVG output from a diagram.
format :: XorY -> Diagram B -> String
format (X x) d = B.unpack $ renderBS $ renderDia SVG (SVGOptions (mkWidth (fromIntegral x)) Nothing "" [] False) d
format (Y y) d = B.unpack $ renderBS $ renderDia SVG (SVGOptions (mkWidth (fromIntegral y)) Nothing "" [] False) d

-- | Handle the error messages from parsing the arguments.
handleParseErrs :: Maybe Int 
    -> Maybe Int
    -> Maybe [Int] 
    -> Maybe Int
    -> Maybe Int
    -> Either String (Int, Int, NonEmpty Int, XorY)
handleParseErrs period frets tuning x y
  -- Valid format
  | Just period' <- period
  , Just frets' <- frets
  , Just tuning' <- nonEmpty =<< tuning
  , isJust x `xor` isJust y
  = case (x,y) of
       (Just x',_) -> Right (period',frets',tuning',X x')
       (_,Just y') -> Right (period',frets',tuning',Y y')
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