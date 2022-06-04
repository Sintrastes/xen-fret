
module Reflex.Dom.Extras where

import Reflex.Dom.Core
import qualified Data.Text as T
import Data.Functor
import Control.Monad
import Control.Applicative (liftA2)
import Control.Monad.Fix
import Data.List.NonEmpty hiding (fromList)
import Data.MultiMap (fromList, toMap)
import Data.Map (Map, empty)
import Data.Aeson.TH
import Data.Aeson
import Control.Monad.IO.Class
import Data.Ratio
import Data.Default
import Data.Profunctor
import Data.Functor.Compose
import XenFret.AppData
import Language.Javascript.JSaddle (eval, liftJSM)

type Form t m a b
    = Star (Compose m (Dynamic t)) a b

type SForm t m a = Form t m a a

form :: DomBuilder t m => (a -> m (Dynamic t b)) -> Form t m a b
form f = Star (Compose . f)

initForm :: DomBuilder t m => Form t m a b -> a -> m (Dynamic t b)
initForm (Star f) x = getCompose $ f x

(=.) :: Profunctor f => (x -> y) -> f y a -> f x a
(=.) = lmap

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

textEntry :: _ => T.Text -> m (Dynamic t T.Text)
textEntry initialValue =
    _inputElement_value <$> inputElement (
        def & inputElementConfig_elementConfig
            . elementConfig_initialAttributes
            .~ attrs
            & inputElementConfig_initialValue .~ initialValue)
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
            .~ attrs
            & inputElementConfig_initialValue
            .~ (T.pack $ show initialValue))
  where
    attrs = "class" =: "p-form-text p-form-no-validate" <>
        "type" =: "number" <>
        "step" =: "1" <>
        "min" =: "0"

rationalEntry :: _ => Rational -> m (Dynamic t Rational)
rationalEntry initialValue = el "div" $ do
    num <- elAttr "div" ("style" =: "display: inline-block;width:60px;margin-right: 7.5px;") $ 
        positiveIntEntry $ fromIntegral $ numerator initialValue
    el "span" $ text "/"
    denom <- elAttr "div" ("style" =: "display: inline-block;width:60px;margin-left: 5px;") $ 
        positiveIntEntry $ fromIntegral $ denominator initialValue
    pure $ liftA2 (%) (toInteger <$> num) (toInteger <$> denom)

button label = do
    let attributes = "class" =: "waves-effect waves-light btn"
    domEvent Click . fst <$> elAttr' "a" attributes
      (text label)

selectMaterial :: (Eq a, Reflex t, MonadHold t m, MonadWidget t m) => Show a => T.Text -> (Dynamic t [a]) -> a -> m (Dynamic t a)
selectMaterial label itemsDyn initialValue = elClass "div" "input-field col s12" $ mdo
    initialItems <- sample $ current itemsDyn

    let initialValueActual = if (initialValue `elem` initialItems) 
        then initialValue
        else Prelude.head initialItems

    (form, changeSelection) <- elClass "div" "select-wrapper" $ do
        (form, _) <- el' "div" $ inputElement $ def
            & inputElementConfig_initialValue .~ (T.pack $ show initialValueActual)
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

    dynResult <- foldDyn const initialValueActual
        (leftmost [changeSelection, itemsUpdated])

    pure dynResult

elSvg tag a1 a2 = do
  elDynAttrNS' (Just "http://www.w3.org/2000/svg") tag (constDyn a1) a2
  return ()

liftFrontend d x = do
    res <- current <$> prerender (pure d) (liftIO x)
    sample res

liftFrontend' d x = do
    res <- current <$> prerender (pure d) x
    sample res

modalHeader :: _ => T.Text -> m ()
modalHeader txt = do
    elAttr "h5" ("style" =: "margin-top: 0em; margin-bottom:1em;") $ text txt
    pure ()

-- | Helper function to open a simple Ok/Cancel modal dialog.
modal :: (MonadFix m, PostBuild t m, MonadHold t m, DomBuilder t m)
      => Event t () -> m (Dynamic t a) -> m (Event t (Maybe a))
modal onClick contents = mdo
    (res, onCancel, onSubmit) <- elDynAttr "div" modalAttrs $ el "section" $ do
        res <- elClass "div" "modal-content"
            contents

        let okAttrs = "class" =: "modal-close waves-effect waves-green btn-flat" <>
                "data-role" =: "button"

        let cancelAttrs = "class" =: "negative modal-close waves-effect waves-green btn-flat" <>
                "data-role" =: "button"

        (onCancel, onSubmit) <- elClass "div" "modal-footer p-modal-button-container" $ do
            onCancel <- domEvent Click . fst <$>
                elAttr' "a" cancelAttrs
                    (text "Cancel")
            onSubmit <- domEvent Click . fst <$>
                elAttr' "a" okAttrs
                    (text "Ok")

            pure (onCancel, onSubmit)

        pure (res, onCancel, onSubmit)

    let events = leftmost
            [
                Open   <$ onClick,
                Closed <$ onCancel,
                Closed <$ onSubmit
            ]

    modalVisibility <- foldDyn const Closed events

    elDynAttr "div" overlayAttrs $ pure ()

    let modalAttrs = modalVisibility <&> \case
            Closed -> "style" =: "display: none;"
            Open   -> "class" =: "modal open" <> "style" =: ("overflow: visible;" <> "z-index: 1003;" <>
                    "display: block;" <> "background-color: transparent;" <>
                    "top: 10%;" <> "transform: scaleX(1) scaleY(1);")
    
    let overlayAttrs = modalVisibility <&> \case
            Open -> "class" =: "modal-overlay" <>
                "style" =: "z-index: 1002; display: block; opacity: 0.5;"
            _ -> empty

    pure $ leftmost
      [
        Just <$> tag (current res) onSubmit
      , Nothing <$ onCancel
      ]

data ModalEvent =
      Open
    | Closed

toast message = do
    liftJSM $ eval ("console.log(\"toast\"); M.toast({html: '" <> message <> "'})" :: T.Text)
    pure ()

toastOnErrors x = do
    res <- x
    case res of
        Left  e ->
            liftFrontend' () $
                toast $ "An exception occured when loading Bedelibry: " <> T.pack (show e)
        Right _ ->
            pure ()