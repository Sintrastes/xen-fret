
module Utils where

import Reflex.Dom.Core
import qualified Data.Text as T
import Data.Functor
import Control.Monad
import Control.Monad.Fix
import Data.List.NonEmpty hiding (fromList)
import Data.MultiMap (fromList, toMap)
import Data.Map (Map, empty)
import Data.Aeson.TH
import Data.Aeson

data Temperament = Temperament {
    temperamentName :: String, 
    period :: Int
}

instance Show Temperament where
    show = temperamentName

$(deriveJSON defaultOptions ''Temperament)

data Tuning = Tuning {
    tuningName :: String,
    instrument :: String,
    stringTunings :: NonEmpty Int
}

instance Show Tuning where
    show = tuningName

$(deriveJSON defaultOptions ''Tuning)

data Scale = Scale {
    scaleName  :: String,
    scaleNotes :: NonEmpty Int
}

instance Show Scale where
    show = scaleName

$(deriveJSON defaultOptions ''Scale)

data PreferenceData = PreferenceData {
    useDarkMode :: Bool
}

$(deriveJSON defaultOptions ''PreferenceData)

data AppData = AppData {
    -- | Get the list of temperaments
    temperaments :: [Temperament],
    -- | Get the tunings associated with a temperament.
    tunings :: Map String [Tuning],
    -- | Get the scales associated with a temperament.
    scales  :: Map String [Scale],
    preferences :: PreferenceData
}

$(deriveJSON defaultOptions ''AppData)

defaultAppData = AppData {
      temperaments = 
        [
            Temperament "11-TET" 11,
            Temperament "12-TET" 12,
            Temperament "13-TET" 13,
            Temperament "16-TET" 16,
            Temperament "19-TET" 19,
            Temperament "22-TET" 22,
            Temperament "24-TET" 24,
            Temperament "Bohlen Pierce" 13
        ]
    , tunings = toMap $ fromList 
        [
            ("12-TET", Tuning "Standard Tuning" "Six-String Guitar" 
                (0 :| [5, 10, 15, 19, 24])),
            ("22-TET", Tuning "Standard Tuning (22-TET)" "Six-String Guitar"
                (0 :| [9, 18, 27, 35, 44]))
        ]
    , scales = toMap $ fromList 
        [
            ("11-TET", Scale "Orgone[7]"
                (1 :| [2, 1, 2, 1, 2, 2]))
          , ("12-TET", Scale "Ionian (Major)" 
                (2 :| [2,1,2,2,2,1]))
          , ("12-TET", Scale "Mixolydian" 
                (2 :| [2,1,2,2,1,2]))
          , ("12-TET", Scale "Minor" 
                (2 :| [1,2,2,1,2,2]))
          , ("12-TET", Scale "Dorian" 
                (2 :| [3,5,7,9,10]))
          , ("13-TET", Scale "Archeotonic (Ryonian Mode)"
                (2 :| [2,2,2,2,2,1]))
          , ("13-TET", Scale "Oneirotonic (Dylathian Mode)"
                (2 :| [2,1,2,2,1,2,1]))
          , ("16-TET", Scale "mavila[7]"
                (2 :| [2,2,2,2,2,1]))
          , ("16-TET", Scale "Lemba"
                (2 :| [2,2,2,2,2,1]))
          , ("16-TET", Scale "Magic"
                (2 :| [2,2,2,2,2,1]))
          , ("19-TET", Scale "Ionian (Major)"
                (2 :| [2,1,2,2,2,1]))
          , ("22-TET", Scale "Ionian (Major)"
                (2 :| [2,1,2,2,2,1]))
          , ("24-TET", Scale "Ionian (Major)"
                (2 :| [2,1,2,2,2,1]))
          , ("Bohlen Pierce", Scale "Lambda"
                (2 :| [2,1,2,2,2,1]))
        ]
    , preferences = defaultPreferences
}

defaultPreferences = PreferenceData {
    useDarkMode = False
}

-- | Nav bar widget.
materialNavBar :: (DomBuilder t m, MonadHold t m, MonadFix m, Show e, PostBuild t m) => [e] -> m (Event t e)
materialNavBar tabs = mdo

    -- Nav bar menu
    (navPane, navPaneEvents) <- elDynAttr' "div" sidebarAttrs $ el "ul" $ do
        tabEvents        <- forM tabs (\tab -> do
            events <- sidebarButton (T.pack $ show tab)
            pure $ tab <$ events)

        pure $ leftmost tabEvents

    (navBarEvents, toggleMenuEvent) <- elAttr "div" ("style" =: "user-select: none;") $ elAttr "nav" ("class" =: "unselectable nav-wrapper") $ el "div" $ do
        navMenu <- elAttr' "a" ("class" =: "unselectable-btn sidenav-trigger" <> "unselectable" =: "on") $
            elClass "i" "material-icons" $ text "menu"

        elClass "a" "brand-logo" $ text "Xen Fret"
        
        elAttr "ul" ("id" =: "nav-mobile" <> "class" =: "right hide-on-med-and-down") $ do
            menuEvents <- forM tabs (\tab -> do
                btnEvents <- navButton (T.pack $ show tab)
                pure $ tab <$ btnEvents)

            pure (leftmost menuEvents, domEvent Click (fst navMenu))

    let outsideNavBarEvents = difference (domEvent Click overlay)
            (domEvent Click navPane)

    sidebarOpened <- accumDyn (\s _ -> not s) False
        (leftmost [() <$ toggleMenuEvent, () <$ navPaneEvents, () <$ outsideNavBarEvents])

    (overlay, _) <- elDynAttr' "div" overlayAttrs blank

    let sidebarAttrs = sidebarOpened <&> \isOpened ->
          "class" =: "w3-sidebar w3-bar-block w3-border-right" <>
            if isOpened
                then "style" =: "display: block; z-index: 999;"
                else "style" =: "display: none;"
    
    let overlayAttrs = sidebarOpened <&> \isOpened ->
          "class" =: "sidenav-overlay" <>
            if isOpened 
                then "style" =: "display: block; opacity: 1;"
                else "style" =: "display: none;"

    pure $ leftmost [navBarEvents, navPaneEvents]

sidebarButton x = el "li" $
    domEvent Click . fst <$>
        elClass' "a" "unselectable w3-bar-item w3-button"
            (text x)

navButton x = el "li" $
    domEvent Click . fst <$>
        el' "a" (text x)

textEntry :: _ => m (InputElement EventResult (DomBuilderSpace m) t)
textEntry =
    inputElement (
        def & inputElementConfig_elementConfig
            . elementConfig_initialAttributes
            .~ attrs)
  where
    attrs = "class" =: "p-form-text p-form-no-validate" <>
        "type" =: "text"

labeledEntry :: _ => T.Text -> (a -> m (Dynamic t a)) -> a -> m (Dynamic t a)
labeledEntry label widget initialValue = elClass "div" "input-field col" $ do
    res <- widget initialValue
    elAttr "label" ("class" =: "active" <> "style" =: "left: 0rem;") $
        text label
    pure res

intEntry :: _ => Int -> m (Dynamic t Int)
intEntry initialValue =
    fmap (read @Int . T.unpack) <$> _inputElement_value <$> inputElement (
        def & inputElementConfig_elementConfig
            . elementConfig_initialAttributes
            .~ attrs
            & inputElementConfig_initialValue
            .~ (T.pack $ show initialValue))
  where
    attrs = "class" =: "p-form-text p-form-no-validate" <>
        "type" =: "number" <>
        "step" =: "1"

positiveIntEntry :: _ => Int -> m (Dynamic t Int)
positiveIntEntry initialValue =
    fmap (read @Int . T.unpack) <$> _inputElement_value <$> inputElement (
        def & inputElementConfig_elementConfig
            . elementConfig_initialAttributes
            .~ attrs)
  where
    attrs = "class" =: "p-form-text p-form-no-validate" <>
        "type" =: "number" <>
        "step" =: "1" <>
        "min" =: "0"

button label = do
    let attributes = "class" =: "waves-effect waves-light btn"
    domEvent Click . fst <$> elAttr' "a" attributes
      (text label)

selectMaterial :: (Reflex t, MonadHold t m, MonadWidget t m) => Show a => T.Text -> (Dynamic t [a]) -> a -> m (Dynamic t a)
selectMaterial label itemsDyn initialValue = elClass "div" "input-field col s12" $ mdo
    (form, changeSelection) <- elClass "div" "select-wrapper" $ do
        (form, _) <- el' "div" $ inputElement $ def
            & inputElementConfig_initialValue .~ T.pack (show initialValue)
            & inputElementConfig_setValue .~ (leftmost [T.pack . show <$> changeSelection, T.pack . show <$> itemsUpdated])

        changeSelection <- elDynAttr "ul" selectAttrs $ do
            itemEvents <- dyn $ itemsDyn <&> \items -> leftmost <$> forM items (\item -> do
                el "li" $
                   (item <$) . domEvent Click . fst <$> el' "span" (
                        text $ T.pack $ show item))
            switchHold never itemEvents

        elSvg "svg" ("class" =: "caret" <>
            "height" =: "24" <>
            "viewBox" =: "0 0 24 24" <>
            "width" =: "24" <>
            "xmlns" =: "http://www.w3.org/2000/svg") $ do
                elSvg "path" ("d" =: "M7 10l5 5 5-5z") $ pure ()
                elSvg "path" ("d" =: "M0 0h24v24H0z" <> "fill" =: "none") $ pure ()

        pure (form, changeSelection)

    elAttr "label" ("style" =: "left: 0rem;") $ text label

    let itemsUpdated = updated $ Prelude.head <$> itemsDyn

    let selectedStyle = "display: block;" <>
          "width: 100%;" <> "left: 0px;" <>
          "top: 0px;" <> "height: auto;" <> "transform-origin: 0px 0px;" <>
          "opacity: 1;" <> "transform: scaleX(1) scaleY(1);"

    let inputClicks = form &
            domEvent Click

    dropdownOpenDyn <- foldDyn const False $
        leftmost [
            True <$ inputClicks,
            False <$ changeSelection
        ]

    let selectAttrs = dropdownOpenDyn <&> \dropdownOpen ->
            "class" =: "dropdown-content select-dropdown" <>
                if dropdownOpen
                    then "style" =: selectedStyle
                    else empty

    dynResult <- foldDyn const initialValue
        (leftmost [changeSelection, itemsUpdated])

    pure dynResult

elSvg tag a1 a2 = do
  elDynAttrNS' (Just "http://www.w3.org/2000/svg") tag (constDyn a1) a2
  return ()