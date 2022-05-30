
module Utils where

import Reflex.Dom.Core
import qualified Data.Text as T
import Data.Functor
import Control.Monad
import Control.Monad.Fix
import Data.List.NonEmpty
import Data.Map (Map)
import Data.Aeson.TH
import Data.Aeson

data Temperament = Temperament String Int

$(deriveJSON defaultOptions ''Temperament)

data Tuning = Tuning {
    tuningName :: String,
    stringTunings :: NonEmpty Int
}

$(deriveJSON defaultOptions ''Tuning)

data Scale = Scale {
    scaleName  :: String,
    scaleNotes :: NonEmpty Int
}

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
        
        elAttr "ul" ("id" =: "nav-mobile" <> "class" =: "left hide-on-med-and-down") $ do
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