
module XenFret.App.Pages.Main where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.List.NonEmpty (NonEmpty(..))
import Data.Validation
import qualified Data.List.NonEmpty as NE
import Language.Javascript.JSaddle (liftJSM, jsg, jsg0, jsg3, jsg1, fromJSVal)
import XenFret.App.Widgets.Fretboard
import XenFret.Data
import XenFret.AppData
import XenFret.Util
import XenFret.App.Util
import XenFret.AppData
import Reflex.Dom.Core hiding(Home, button, checkbox)
import Reflex.Dom.Extras
import Reflex.Dom.Forms
import qualified Data.Sequence as Seq
import Data.List
import Data.Maybe hiding (mapMaybe)
import Data.Functor
import Control.Monad
import Data.Foldable
import XenFret.App.Widgets.Fretboard
import XenFret
import GHC.Float

baseVerticalSpacing :: Double
baseVerticalSpacing = 0.2

baseHorizontalSpacing :: Double
baseHorizontalSpacing = 0.5/5

mainPage :: _ => FilePath -> m ()
mainPage appDir = do
    appData <- loadAppData (appDir <> "/app_data.json")

    elAttr "div" ("style" =: "display: flex;height:100%;") $ do
        (saveEvent, viewDiagramEvent, dynArgs) <- elClass "div" "main-pane-left" $ do
            currentTab <- tabSwitcher ["Scale Diagram", "Chord Diagram"] "Scale Diagram"

            initialTab <- sample $ current currentTab

            let getSelectForm = \x y -> \case
                  "Scale Diagram" -> scaleSelectForm x y
                  "Chord Diagram" -> chordSelectForm x y

            let selectForm = \x y -> join <$> widgetHold 
                  ((getSelectForm x y) initialTab)
                  (getSelectForm x y <$> updated currentTab)

            elClass "div" "pane-body" $ do
                diagramOptionsWidget selectForm appData

        diagramUpdated <- elClass "div" "main-pane-right" $ do
            fretboardDisplayWidget dynArgs

        mobileSaveEvent <- modalWidget' "top: 2.5%;" viewDiagramEvent $ do
            res <- button "Save"
            fretboardDisplayWidget dynArgs
            pure res

        diagramDyn <- holdDyn Nothing
            diagramUpdated

        let saveEvents = leftmost [saveEvent, switch $ current mobileSaveEvent]

        _ <- prerender (pure never) $ performEvent $ saveEvents <&> \_ -> do
            maybeSvgText <- sample $ current diagramDyn
            case maybeSvgText of
                Just svgText -> do
                    _ <- liftJSM $ jsg3 ("download" :: T.Text) svgText
                        ("diagram.svg" :: T.Text)
                        ("image/svg" :: T.Text)
                    pure ()
                Nothing ->
                    toast "Invalid diagram. Cannot save."

        blank

scaleSelectForm appData temperamentDyn = do
    let initialTemperament = temperamentName $ head $ temperaments appData
    let initialScales = maybe [] id $ 
            Map.lookup initialTemperament $ scales appData

    let loadedScales = temperamentDyn <&> (\temperamentMay -> maybe [] id $ do
            temperament <- temperamentMay
            Map.lookup (temperamentName temperament) $ scales appData)

    selectMaterial "Scale"
        "No Scales Defined"
        loadedScales
        (head initialScales)

chordSelectForm appData temperamentDyn = do
    let initialTemperament = temperamentName $ head $ temperaments appData
    let initialChords = maybe [] id $ 
            Map.lookup initialTemperament $ chords appData

    -- Convert to scale, as that is the format the
    -- diagram display widget understands
    fmap (toScale <$>) <$> selectMaterial "Chord"
        "No Chords Defined"
        (pure initialChords)
        (head initialChords)

diagramOptionsWidget selectForm appData = do
    let initialTemperament = head $ temperaments appData
    temperamentDyn <- elClass "div" "row" $
        selectTemperament appData initialTemperament

    let initialTunings = maybe [] id $ Map.lookup (temperamentName initialTemperament) $ tunings appData
    
    let loadedTunings = temperamentDyn <&> (\temperamentMay -> maybe [] id $ do
            temperament <- temperamentMay
            Map.lookup (temperamentName temperament) $ tunings appData)

    let groupedTunings = loadedTunings <&> (\tunings ->
            let groupings = groupBy (\x y -> instrument x == instrument y) tunings
            in (\grouping -> (instrument $ head grouping, grouping)) <$> groupings)

    tuningDyn <- elClass "div" "row" $
        selectOptgroups "Instrument/Tuning"
            "No Tunings Defined"
            groupedTunings
            (head initialTunings)

    scaleDyn <- elClass "div" "row" 
        (selectForm appData temperamentDyn)

    (keyDyn, offsetDyn) <- elAttr "div" ("class" =: "row" <> "style" =: "margin-bottom: 0px;") $ do
        keyDyn <- elAttr "div" ("class" =: "col s6" <> "style" =: "padding-left: 0px;") $
            labeledEntry "Key" positiveIntEntry 0
        offsetDyn <- elAttr "div" ("class" =: "col s6" <> "style" =: "padding-right: 0px;") $
            labeledEntry "Fret Offset" positiveIntEntry 0
        pure (keyDyn, offsetDyn)

    elAttr "h5" ("class" =: "unselectable" <> "style" =: "padding-bottom: 10px;") $ text "Display Options:"

    (sizeDyn, fretsDyn) <- elClass "div" "row" $ do
        sizeDyn  <- elAttr "div" ("class" =: "col s6" <> "style" =: "padding-left: 0px;") $
            labeledEntry "Size" intEntry 264
        fretsDyn <- elAttr "div" ("class" =: "col s6" <> "style" =: "padding-right: 0px;") $
            labeledEntry "Number of Frets" intEntry 10
        pure (sizeDyn, fretsDyn)

    (verticalScalingDyn, horizontalScalingDyn) <- elAttr "div" ("class" =: "row" <> "style" =: "margin-bottom: 0px;") $ do
        verticalScalingDyn   <- elAttr "div" ("class" =: "col s6" <> "style" =: "padding-left: 0px;") $
            labeledEntry "Vertical Spacing" intEntry 200
        horizontalScalingDyn <- elAttr "div" ("class" =: "col s6" <> "style" =: "padding-right: 0px;") $
            labeledEntry "Horizontal Spacing" intEntry 331
        pure (verticalScalingDyn, horizontalScalingDyn)

    elClass "div" "col s12" $
        blank
        -- checkbox "Display vertically" True

    elClass "div" "col s12" $
        blank
        -- checkbox "Use realistic fret spacing" False

    displayMarkersOnFretsDyn <- elClass "div" "col s12" $
        pure $ pure True
        -- checkbox "Display markers on frets" False

    saveEvent <- elClass "div" "show-when-pane-open" $
        button "Save"

    viewDiagramEvent <- elClass "div" "hide-when-pane-open" $
        button "View Diagram"

    pure (saveEvent, viewDiagramEvent, (,,,,,,,,,) <$>
        fretsDyn <*>
        sizeDyn <*>
        scaleDyn <*>
        temperamentDyn <*>
        verticalScalingDyn <*>
        horizontalScalingDyn <*>
        keyDyn <*>
        offsetDyn <*>
        tuningDyn <*>
        displayMarkersOnFretsDyn)

fretboardDisplayWidget dynArgs = dyn $ dynArgs <&>
    \(frets, xSize, scale,
      temperament, verticalScaling,
      horizontalScaling, key, offset,
      tuning, displayMarkersOnFrets) ->
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
                Right (fretboard, _) -> elAttr "div" ("style" =: "text-align: center;") $ do
                    let Just scalePeriod = sum . scaleIntervals <$> scale
                    let Just (scaleRoot NE.:| _) = scaleIntervals <$> scale
                    let diagram = board displayMarkersOnFrets
                            (maybe "" show scale) offset scalePeriod key
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