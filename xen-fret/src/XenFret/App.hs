{-# OPTIONS_GHC -Wno-name-shadowing #-}

module XenFret.App where

import Data.Maybe (isJust, isNothing, fromJust, fromMaybe)
import Control.Monad
import Diagrams.Prelude ( renderDia, mkWidth, Diagram )
import Diagrams.Backend.SVG
import qualified Data.ByteString.Lazy.Char8 as B
import XenFret.Util
import XenFret
import Graphics.Svg
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Reflex.Dom.Core hiding(Home, button, checkbox)
import Reflex.Dom.Extras
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Functor
import qualified Data.Map as Map
import qualified Data.List.NonEmpty as NE
import Control.Exception
import System.Info
import System.Directory
import Data.List hiding (transpose)
import Data.Aeson hiding (Success)
import Data.Aeson.Casing
import Control.Monad.IO.Class
import GHC.Float
import Language.Javascript.JSaddle (liftJSM, jsg, jsg0, jsg3, jsg1, fromJSVal)
import XenFret.Data
import XenFret.AppData
import GHC.Generics
import qualified Language.Javascript.JSaddle as JS
import Data.ByteString.Lazy (toStrict)
import Debug.Trace (traceIO, trace)
import Reflex.Dom.Extras
import Reflex.Dom.Forms
import Data.Validation
import XenFret.App.Util
import Control.Monad.Fix
import Data.Map (Map, lookup, insert)

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

loadAppData :: (MonadSample t m, Prerender t m, MonadIO m) => FilePath -> m AppData
#ifdef ghcjs_HOST_OS
loadAppData _ = liftIO $ JS.catch (do
    cookieData <- liftJSM $ jsg0 ("getAppData" :: T.Text)
    (rawText :: Maybe T.Text) <- fromJSVal cookieData
    case rawText of 
        Nothing -> pure $ defaultAppData
        Just rawText' -> do
            liftIO $ traceIO $ T.unpack rawText'
            pure $ maybe defaultAppData id $ decodeStrict (encodeUtf8 rawText'))
    (\(_ :: SomeException) -> pure defaultAppData)
#else
loadAppData dataFile = do
    loadedData :: AppData <- liftFrontend defaultAppData $
        catch (fromJust <$> decodeFileStrict dataFile)
            (\(_ :: SomeException) -> return defaultAppData)
    pure loadedData
#endif

loadAppData' :: (MonadSample t m, MonadIO m) => FilePath -> m AppData
#ifdef ghcjs_HOST_OS
loadAppData' _ = liftIO $ JS.catch (do
    cookieData <- liftJSM $ jsg0 ("getAppData" :: T.Text)
    (rawText :: Maybe T.Text) <- fromJSVal cookieData
    case rawText of 
        Nothing -> pure $ defaultAppData
        Just rawText' -> do
            liftIO $ traceIO $ T.unpack rawText'
            pure $ maybe defaultAppData id $ decodeStrict (encodeUtf8 rawText'))
    (\(_ :: SomeException) -> pure defaultAppData)
#else
loadAppData' dataFile = liftIO $ catch (fromJust <$> decodeFileStrict dataFile)
    (\(_ :: SomeException) -> return defaultAppData)
#endif

persistAppData :: (ToJSON a, Applicative m, Prerender t m, Monad m ) =>
  Dynamic t a -> FilePath -> m ()
#ifdef ghcjs_HOST_OS
persistAppData dynAppData dataFile = do
    prerender (pure never) $ performEvent $ updated dynAppData <&>
        \newData -> do
            liftJSM $ jsg1 ("storeAppData" :: T.Text)
                (decodeUtf8 $ toStrict $ encode newData)
    pure ()
#else
persistAppData dynAppData dataFile = do
    prerender (pure never) $ performEvent $ updated dynAppData <&>
        \newData ->
            liftIO $ encodeFile dataFile newData
    pure ()
#endif

validateNonEmpty :: T.Text -> Validation (NonEmpty T.Text) T.Text
validateNonEmpty x
    | x == ""   = Failure ("String must not be empty" :| [])
    | otherwise = Success x

app :: _ => m ()
app = do
    -- Setup the application directory.
    appDir <- if "android" `isInfixOf` os
        then pure "/data/data/org.xenfret.app"
        else liftFrontend "/" getHomeDirectory <&> (<> "/.xenfret")

    _ <- liftFrontend (Right ()) $ catch
        (do createDirectoryIfMissing True appDir
            pure $ Right ())
        (\(e :: SomeException) -> pure $ Left e)

    navEvents <- materialNavBar [Home, Temperaments, Tunings, Scales, Preferences] $
        githubWidget

    currentPage <- holdDyn Home navEvents

    _ <- dyn $ currentPage <&> \case
        Home -> mainPage appDir
        Temperaments -> temperamentPage appDir
        Tunings -> tuningPage appDir
        Scales -> scalePage appDir
        Preferences -> preferencePage appDir
    blank

selectTemperament :: _ => AppData -> Temperament -> m (Dynamic t (Maybe Temperament))
selectTemperament appData temperament = do
    let loadedTemperaments = temperaments appData

    selectMaterial "Temperament"
        "No Temperaments Defined"
        (pure loadedTemperaments)
        temperament

mainPage :: _ => FilePath -> m ()
mainPage appDir = do
    appData <- loadAppData (appDir <> "/app_data.json")

    elAttr "div" ("style" =: "display: flex;height:100%;") $ do
        (saveEvent, dynArgs) <- elClass "div" "main-pane-left" $ do
            elAttr "h5" ("class" =: "unselectable" <> "style" =: "padding-bottom: 10px;") $ text "Diagram Options:"

            temperamentDyn <- elClass "div" "row" $
                selectTemperament appData
                    (head $ temperaments appData)

            let Just initialTunings = Map.lookup "12-TET" $ tunings appData

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

            pure (saveEvent, (,,,,,,,,,) <$>
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

        diagramUpdated <- elClass "div" "main-pane-right" $ do
            -- Handle errors parsing the arguments
            dyn $ dynArgs <&> 
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

        diagramDyn <- holdDyn Nothing
            diagramUpdated

        _ <- prerender (pure never) $ performEvent $ saveEvent <&> \_ -> do
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

getTemperaments appDir = do
    appData <- loadAppData' (appDir <> "/app_data.json")
    return $ temperaments appData

temperamentPage :: _ => FilePath -> m ()
temperamentPage appDir = mdo
    appData <- loadAppData (appDir <> "/app_data.json")
    let initialTemperaments = temperaments appData

    newTemperamentEvent <- button "New Temperament"

    updatedTemperaments <- switch . current <$> prerender (pure never) (performEvent $ newTemperamentSubmitted <&> \case
        Nothing -> pure initialTemperaments
        Just temperament -> do
            toast "Added new temperament"
            currentTemperament <- getTemperaments appDir
            pure (currentTemperament ++ [temperament]))

    dynTemperaments <- holdDyn initialTemperaments
        updatedTemperaments

    let isNewName = dynTemperaments <&> \ temperaments name ->
            let names = fmap temperamentName temperaments in
                if name `elem` names
                    then Failure $ "There is already a temperament with this name." :| []
                    else Success name

    newTemperamentSubmitted <- validatedModal newTemperamentEvent $
        temperamentForm isNewName def

    let dynAppData = dynTemperaments <&> \t ->
            appData { temperaments = t }

    persistAppData dynAppData
        (appDir <> "/app_data.json")

    _ <- dyn $ dynTemperaments <&> \currentTemperaments ->
        elClass "ul" "collection" $ do
            forM_ currentTemperaments (\temperament -> do
                elClass "li" "collection-item" $ do
                    el "span" $ text $
                        T.pack $ show temperament)
    blank

temperamentForm :: _ =>
     Dynamic t (T.Text -> Validation (NonEmpty ErrorMessage) T.Text)
  -> Temperament
  -> m (Dynamic t (Validation (NonEmpty ErrorMessage) Temperament))
temperamentForm isNewName initialValue = do
    modalHeader "Add New Temperament"

    let nameValidation = combineDynValidations [
                pure (nonEmptyText "Name cannot be blank")
              , isNewName
          ]

    let nameForm = validatedTextEntryDyn nameValidation id

    let formContents = Temperament <$>
            formA (temperamentName =. labeledEntryA "Name" nameForm) <*>
            liftFormA (form (divisions =. labeledEntry "Divisions" nonNegativeIntEntry)) <*>
            liftFormA (form (period =. labeledEntry "Period" rationalEntry)) <*>
            pure Nothing

    initFormA formContents initialValue

getTunings :: (MonadSample t m, MonadIO m) => FilePath -> m (Map.Map T.Text [Tuning])
getTunings appDir = do
    appData <- loadAppData' (appDir <> "/app_data.json")
    return $ tunings appData

tuningPage :: _ => FilePath -> m ()
tuningPage appDir = mdo
    appData <- loadAppData (appDir <> "/app_data.json")
    let initialTunings = tunings appData

    newTuningEvent <- button "New Tuning"

    updatedTunings <- switch . current <$> prerender (pure never) (performEvent $ newTuningSubmitted <&> \case
        Nothing -> getTunings appDir
        Just (temperament, tuning) -> do
            toast "Added new tuning"
            currentTunings <- getTunings appDir
            pure $ addTuning temperament tuning currentTunings)

    dynTunings <- holdDyn initialTunings
        updatedTunings

    let isNewName = dynTunings <&> \tunings temperament instrument name ->
          let names = fmap tuningName (fromMaybe [] $ Map.lookup (temperamentName temperament) tunings) in
            if name `elem` names
                then Failure $ ("There is already a " <> instrument <>
                        " tuning with this name for " <> temperamentName temperament <> ".") :| []
                else Success name

    newTuningSubmitted <- validatedModal newTuningEvent $ do
        tuningForm appData isNewName def

    let dynAppData = dynTunings <&> \t ->
           appData { tunings = t }

    persistAppData dynAppData
       (appDir <> "/app_data.json")

    _ <- dyn $ dynTunings <&> \currentTunings ->
        elClass "ul" "collection" $ do
            forM_ (join $ Map.elems $ currentTunings) (\tuning -> do
                elClass "li" "collection-item" $ do
                    el "span" $ text $
                        T.pack $ show tuning)
    blank

-- | Helper function to add a tuning to the given temperament.
addTuning :: Temperament -> Tuning -> Map T.Text [Tuning] -> Map T.Text [Tuning]
addTuning Temperament{..} tuning map = case Data.Map.lookup temperamentName map of
  Nothing -> Data.Map.insert temperamentName [tuning] map
  Just tuns -> Data.Map.insert temperamentName (tuns ++ [tuning]) map

tuningForm :: MonadWidget t m => AppData
    -> Dynamic t (Temperament -> T.Text -> T.Text -> Validation (NonEmpty ErrorMessage) T.Text)
    -> Tuning
    -> m (
        Dynamic t (Validation (NonEmpty ErrorMessage) (Temperament, Tuning))
    )
tuningForm appData isNewName initialValue = do
    modalHeader "Add New Tuning"

    let initialTemperament = head $ temperaments appData

    temperamentForm <- (validateNonNull "Temperament must be selected" <$>) <$>
            selectTemperament appData initialTemperament

    let temperamentUpdated = fmapMaybe validationToMaybe
            (updated temperamentForm)

    currentTemperament <- holdDyn initialTemperament =<<
        delay 0.1 temperamentUpdated

    let currentInstrument = pure ""

    let tuningValidation = combineDynValidations
          [ pure $ nonEmptyText "Tuning name must not be empty"
          , isNewName <*> currentTemperament <*> currentInstrument]

    nameDyn <- (labeledEntryA "Name" (validatedTextEntryDyn tuningValidation id))
        (tuningName initialValue)

    instrumentDyn <- (labeledEntryA "Instrument" (
          nonEmptyTextEntry
              "Instrument name must not be empty"))
          (instrument initialValue)

    intervalsDyn <- labeledEntryA "Intervals"
        intervalListEntry
        (stringTunings initialValue)    

    let tuningFrom = mkTuningForm
            nameDyn instrumentDyn
            intervalsDyn

    pure $ pairForms temperamentForm tuningFrom

mkTuningForm :: (Semigroup e, Reflex t) =>
     Dynamic t (Validation e T.Text)
  -> Dynamic t (Validation e T.Text)
  -> Dynamic t (Validation e (NonEmpty Int))
  -> Dynamic t (Validation e Tuning)
mkTuningForm name instrument strings = do
    name' <- name
    instrument' <- instrument
    strings' <- strings

    return $ Tuning <$>
        name' <*>
        instrument' <*>
        strings'

scaleForm :: MonadWidget t m =>
     AppData
  -> Dynamic t (Temperament -> T.Text -> Validation (NonEmpty ErrorMessage) T.Text)
  -> Scale
  -> m (Dynamic t (Validation (NonEmpty ErrorMessage) (Temperament, Scale)))
scaleForm appData isValidName initialValue = mdo
    modalHeader "Add New Scale"

    let initialTemperament = head $ temperaments appData

    currentTemperament <- holdDyn initialTemperament =<<
        delay 0.1 temperamentUpdated

    let scaleValidation = combineDynValidations
          [ pure $ nonEmptyText "Scale name must not be entry"
          , isValidName <*> currentTemperament]

    temperamentForm <- (validateNonNull "Temperament must be selected" <$>) <$>
            selectTemperament appData initialTemperament

    let temperamentUpdated = fmapMaybe validationToMaybe
            (updated temperamentForm)

    let periodDyn = currentTemperament <&> divisions

    scaleForm <- initFormA (Scale <$>
          formA (scaleName =. labeledEntryA "Name"
            (validatedTextEntryDyn scaleValidation id)) <*>
          formA (scaleIntervals =. labeledEntryA "Intervals"
            (scaleListEntry periodDyn))) initialValue

    pure $ pairForms temperamentForm scaleForm

pairForms :: (Semigroup e, Reflex t) =>
    Dynamic t (Validation e a)
 -> Dynamic t (Validation e b)
 -> Dynamic t (Validation e (a, b))
pairForms x y = do
    x' <- x
    y' <- y
    pure $ (,) <$> x' <*> y'

validationToMaybe = \case
    Success x -> Just x
    Failure _ -> Nothing

-- | Helper function to add a scale to the given temperament.
addScale :: Temperament -> Scale -> Map T.Text [Scale] -> Map T.Text [Scale]
addScale Temperament{..} scale map = case Data.Map.lookup temperamentName map of
  Nothing -> Data.Map.insert temperamentName [scale] map
  Just scls -> Data.Map.insert temperamentName (scls ++ [scale]) map

getScales :: (MonadSample t m, MonadIO m) => FilePath -> m (Map.Map T.Text [Scale])
getScales appDir = do
    appData <- loadAppData' (appDir <> "/app_data.json")
    return $ scales appData

scalePage :: _ => FilePath -> m ()
scalePage appDir = mdo
    appData <- loadAppData (appDir <> "/app_data.json")
    let currentScales = scales appData

    newScaleClick <- button "New Scale"

    updatedScales <- switch . current <$> prerender (pure never) (performEvent $ newScaleSubmitted <&> \case
        Nothing -> pure currentScales
        Just (temperament, scale) -> do
            toast "Added new temperament"
            currentScales <- getScales appDir
            pure (addScale temperament scale currentScales))

    dynScales <- holdDyn currentScales
        updatedScales

    let isNewName = dynScales <&> \scales temperament name -> trace ("isNewName " ++ show temperament ++ " " ++ show name) $
          let names = fmap scaleName (fromMaybe [] $ Map.lookup (temperamentName temperament) scales) in
            if name `elem` names
                then Failure $ "There is already a scale with this name." :| []
                    else Success name

    newScaleSubmitted <- validatedModal newScaleClick $
        scaleForm appData isNewName def

    let dynAppData = dynScales <&> \s ->
            appData { scales = s }

    persistAppData dynAppData
        (appDir <> "/app_data.json")

    _ <- dyn $ dynScales <&> \currentScales ->
        elClass "ul" "collection" $ do
            forM_ currentScales (\scale -> do
                elClass "li" "collection-item" $ do
                    el "span" $ text $
                        T.pack $ show scale)
    blank

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

-- | Json object for a github api response.
data GithubData = GithubData {
    stargazersCount :: Int,
    forksCount :: Int
} deriving(Generic)

instance ToJSON GithubData where
   toJSON = genericToJSON $ (aesonDrop 0 snakeCase)
instance FromJSON GithubData where
   parseJSON = genericParseJSON $ (aesonDrop 0 snakeCase)

repoUrl :: T.Text
repoUrl = "https://github.com/sintrastes/xen-fret"

repoApiUrl :: T.Text
repoApiUrl = "https://api.github.com/repos/sintrastes/xen-fret"

fetchGithubData :: _ => Event t () -> m (Event t (Maybe GithubData))
fetchGithubData fetchEv = getAndDecode
    (fetchEv $> repoApiUrl)

-- | Widget used to display source information.
-- adapted from material for mkdocs (https://squidfunk.github.io/mkdocs-material/),
-- (MIT licensed)
githubWidget :: _ => m ()
githubWidget = do
    dataFetched <- fetchGithubData =<< getPostBuild

    -- Build up Dyns for our data
    starsDynText <- foldDyn
        (\ghData curr ->
            maybe "ERR" (T.pack . show . stargazersCount) ghData)
        "" dataFetched

    forksDynText <- foldDyn
        (\ghData curr ->
            maybe "ERR" (T.pack . show . forksCount) ghData)
        "" dataFetched

    -- Build the UI
    elAttr "a" ("style" =: "margin-top: -5.5px;" <> "class" =: "right" <> "href" =: repoUrl) $ elAttr "div" ("style" =: "display: block; margin-right: 25px;") $ do
        elAttr "div" ("style" =: "height: 2.4rem;width: 2rem;display: inline-block;vertical-align: middle;") $ gitIcon
        elAttr "div" ("style" =: "vertical-align: top; display: inline-flex;flex-direction: column;align-items: center;justify-content: center;margin-left: -2rem;padding-left: 2.4rem;font-size: .65rem;") $ do
            elAttr "div" ("style" =: "height: 1.5em;") $ text "sintrastes/xen-fret"
            elAttr "div" ("style" =: "height: 1.5em;") $ do
                starsIcon
                stars starsDynText
                forksIcon
                forks forksDynText
  where
    gitIcon :: _ => m ()
    gitIcon = elSvg "svg" ("viewBox" =: "0 0 448 512" <> "xmlns" =: "http://www.w3.org/2000/svg") $
        elSvg "path" ("d" =: "M439.55 236.05 244 40.45a28.87 28.87 0 0 0-40.81 0l-40.66 40.63 51.52 51.52c27.06-9.14 52.68 16.77 43.39 43.68l49.66 49.66c34.23-11.8 61.18 31 35.47 56.69-26.49 26.49-70.21-2.87-56-37.34L240.22 199v121.85c25.3 12.54 22.26 41.85 9.08 55a34.34 34.34 0 0 1-48.55 0c-17.57-17.6-11.07-46.91 11.25-56v-123c-20.8-8.51-24.6-30.74-18.64-45L142.57 101 8.45 235.14a28.86 28.86 0 0 0 0 40.81l195.61 195.6a28.86 28.86 0 0 0 40.8 0l194.69-194.69a28.86 28.86 0 0 0 0-40.81z") blank

    stars starsDynText = dynText starsDynText {- octicons/star-16.svg -}
    forks forksDynText = dynText forksDynText {- octicons/repo-forked-16.svg -}

    forksIcon :: _ => m ()
    forksIcon = elSvg "svg" ("style" =: "margin-left: 0.4rem; height: 0.6rem; width: 0.6rem;" <>"viewBox" =: "0 0 16 16" <> "xmlns" =: "http://www.w3.org/2000/svg") $
        elSvg "path" ("fill-rule" =: "evenodd" <> "d" =: "M5 3.25a.75.75 0 1 1-1.5 0 .75.75 0 0 1 1.5 0zm0 2.122a2.25 2.25 0 1 0-1.5 0v.878A2.25 2.25 0 0 0 5.75 8.5h1.5v2.128a2.251 2.251 0 1 0 1.5 0V8.5h1.5a2.25 2.25 0 0 0 2.25-2.25v-.878a2.25 2.25 0 1 0-1.5 0v.878a.75.75 0 0 1-.75.75h-4.5A.75.75 0 0 1 5 6.25v-.878zm3.75 7.378a.75.75 0 1 1-1.5 0 .75.75 0 0 1 1.5 0zm3-8.75a.75.75 0 1 0 0-1.5.75.75 0 0 0 0 1.5z") blank


    starsIcon :: _ => m ()
    starsIcon = elSvg "svg" ("style" =: "margin-left: 0.4rem; height: 0.6rem; width: 0.6rem;" <>"viewBox" =: "0 0 16 16" <> "xmlns" =: "http://www.w3.org/2000/svg") $
        elSvg "path" ("fill-rule" =: "evenodd" <> "d" =: "M8 .25a.75.75 0 0 1 .673.418l1.882 3.815 4.21.612a.75.75 0 0 1 .416 1.279l-3.046 2.97.719 4.192a.75.75 0 0 1-1.088.791L8 12.347l-3.766 1.98a.75.75 0 0 1-1.088-.79l.72-4.194L.818 6.374a.75.75 0 0 1 .416-1.28l4.21-.611L7.327.668A.75.75 0 0 1 8 .25zm0 2.445L6.615 5.5a.75.75 0 0 1-.564.41l-3.097.45 2.24 2.184a.75.75 0 0 1 .216.664l-.528 3.084 2.769-1.456a.75.75 0 0 1 .698 0l2.77 1.456-.53-3.084a.75.75 0 0 1 .216-.664l2.24-2.183-3.096-.45a.75.75 0 0 1-.564-.41L8 2.694v.001z") blank













