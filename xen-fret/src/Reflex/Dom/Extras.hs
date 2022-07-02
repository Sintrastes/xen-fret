
module Reflex.Dom.Extras where

import Reflex.Dom.Core
import qualified Data.Text as T
import Data.Functor
import Control.Monad
import Control.Applicative (liftA2)
import Control.Monad.Fix
import Data.Map (empty, Map)
import Control.Monad.IO.Class
import Data.Ratio
import Language.Javascript.JSaddle (eval, liftJSM)
import Data.Validation
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

type ErrorMessage = T.Text

-- | Nav bar widget.
materialNavBar :: (DomBuilder t m, MonadHold t m, MonadFix m, Show e, PostBuild t m) => [e] -> m () -> m (Event t e)
materialNavBar tabs extras = mdo

    -- Nav bar menu
    (navPane, navPaneEvents) <- elDynAttr' "div" sidebarAttrs $ el "ul" $ do
        tabEvents        <- forM tabs (\tab -> do
            events <- sidebarButton (T.pack $ show tab)
            pure $ tab <$ events)

        pure $ leftmost tabEvents

    (navBarEvents, toggleMenuEvent) <- elAttr "div" ("style" =: "user-select: none;") $ elAttr "nav" ("class" =: "unselectable nav-wrapper") $ el "div" $ do
        navMenu <- elAttr' "a" ("class" =: "unselectable-btn sidenav-trigger" <> "unselectable" =: "on") $
            elClass "i" "material-icons" $ text "menu"

        extras

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

sidebarButton :: DomBuilder t m => T.Text -> m (Event t ())
sidebarButton x = el "li" $
    domEvent Click . fst <$>
        elClass' "a" "unselectable w3-bar-item w3-button"
            (text x)

navButton :: DomBuilder t m => T.Text -> m (Event t ())
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

nonEmptyTextEntry :: _ => ErrorMessage -> T.Text -> m (Dynamic t (Validation (NonEmpty ErrorMessage) T.Text))
nonEmptyTextEntry errorMsg = validatedTextEntry 
    (\case
        "" -> Failure $ errorMsg :| []
        x -> Success x)
    id

validatedTextEntry :: _ => 
       (T.Text -> Validation (NonEmpty T.Text) a) 
    -> (a -> T.Text)
    -> a 
    -> m (Dynamic t (Validation (NonEmpty T.Text) a))
validatedTextEntry validation display initialValue = el "div" $ mdo
    let initialValidated = validation (display initialValue)

    res <- (validation <$>) . _inputElement_value <$> inputElement (
        def & inputElementConfig_elementConfig
            . elementConfig_initialAttributes
            .~ attrs initialValidated
            & inputElementConfig_elementConfig
            . elementConfig_modifyAttributes 
            .~ attributeUpdates
            & inputElementConfig_initialValue .~ display initialValue)

    let attributeUpdates = updated $ res <&> \case
            Failure _ -> "class" =: Just "p-form-text invalid"
            Success _ -> "class" =: Just "p-form-text valid"

    let validationText = res <&> (\case
           Failure ne -> T.intercalate "\n" (NE.toList ne)
           Success _ -> "")

    elAttr "span" ("style" =: "color: var(--red-accent-color);") $ dynText 
        validationText

    return res
  where
    attrs :: Validation e a -> Map AttributeName T.Text
    attrs = \case
        Success _ -> "class" =: "p-form-text valid" <>
            "type" =: "text"
        Failure _ -> "class" =: "p-form-text invalid" <>
            "type" =: "text"

validatedTextEntryDyn :: _ => 
       Dynamic t (T.Text -> Validation (NonEmpty T.Text) a) 
    -> (a -> T.Text)
    -> a 
    -> m (Dynamic t (Validation (NonEmpty T.Text) a))
validatedTextEntryDyn validationDyn display initialValue = el "div" $ mdo
    initialValidation <- sample $ current validationDyn
    let initialValidated = initialValidation (display initialValue)

    res' <- _inputElement_value <$> inputElement (
        def & inputElementConfig_elementConfig
            . elementConfig_initialAttributes
            .~ attrs initialValidated
            & inputElementConfig_elementConfig
            . elementConfig_modifyAttributes 
            .~ attributeUpdates
            & inputElementConfig_initialValue .~ display initialValue)

    let res = validationDyn <*> res'

    let attributeUpdates = updated $ res <&> \case
            Failure _ -> "class" =: Just "p-form-text invalid"
            Success _ -> "class" =: Just "p-form-text valid"

    let validationText = res <&> (\case
           Failure ne -> T.intercalate "\n" (NE.toList ne)
           Success _ -> "")

    elAttr "span" ("style" =: "color: var(--red-accent-color);") $ dynText 
        validationText

    return res
  where
    attrs :: Validation e a -> Map AttributeName T.Text
    attrs = \case
        Success _ -> "class" =: "p-form-text valid" <>
            "type" =: "text"
        Failure _ -> "class" =: "p-form-text invalid" <>
            "type" =: "text"

-- |  Transforms a form into a "labeled" form, using a materialize style.
-- 
--  See labeledEntryA for a variant for applicative forms.
labeledEntry :: _ => T.Text -> (a -> m (Dynamic t a)) -> a -> m (Dynamic t a)
labeledEntry label widget initialValue = elClass "div" "input-field" $ do
    res <- widget initialValue
    elAttr "label" ("class" =: "active" <> "style" =: "left: 0rem;") $
        text label
    pure res

labeledEntryA :: _ => T.Text -> (a -> m (Dynamic t (f a))) -> a -> m (Dynamic t (f a))
labeledEntryA label widget initialValue = elClass "div" "input-field" $ do
    res <- widget initialValue
    elAttr "label" ("class" =: "active" <> "style" =: "left: 0rem;") $
        text label
    pure res

intEntry :: _ => Int -> m (Dynamic t Int)
intEntry initialValue =
    fmap (read @Int . T.unpack) . _inputElement_value <$> inputElement (
        def & inputElementConfig_elementConfig
            . elementConfig_initialAttributes
            .~ attrs
            & inputElementConfig_initialValue
            .~ T.pack (show initialValue))
  where
    attrs = "class" =: "p-form-text p-form-no-validate" <>
        "type" =: "number" <>
        "step" =: "1"

positiveIntEntry :: _ => Int -> m (Dynamic t Int)
positiveIntEntry initialValue =
    fmap (read @Int . T.unpack) . _inputElement_value <$> inputElement (
        def & inputElementConfig_elementConfig
            . elementConfig_initialAttributes
            .~ attrs
            & inputElementConfig_initialValue
            .~ T.pack (show initialValue))
  where
    attrs = "class" =: "p-form-text p-form-no-validate" <>
        "type" =: "number" <>
        "step" =: "1" <>
        "min" =: "1"

nonNegativeIntEntry :: _ => Int -> m (Dynamic t Int)
nonNegativeIntEntry initialValue =
    fmap (read @Int . T.unpack) . _inputElement_value <$> inputElement (
        def & inputElementConfig_elementConfig
            . elementConfig_initialAttributes
            .~ attrs
            & inputElementConfig_initialValue
            .~ T.pack (show initialValue))
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

button :: DomBuilder t f => T.Text -> f (Event t ())
button label = do
    let attributes = "class" =: "waves-effect waves-light btn"
    domEvent Click . fst <$> elAttr' "a" attributes
      (text label)

selectMaterial :: (Eq a, Reflex t, MonadHold t m, MonadWidget t m, Show a) =>
     T.Text
  -> T.Text
  -> Dynamic t [a]
  -> a -> m (Dynamic t (Maybe a))
selectMaterial label missingText itemsDyn initialValue = elClass "div" "input-field" $ mdo
    initialItems <- sample $ current itemsDyn

    let initialValueActual = if initialValue `elem` initialItems
        then Just initialValue
        else headMay initialItems

    (form, changeSelection) <- elClass "div" "select-wrapper" $ do
        (form, _) <- el' "div" $ inputElement $ def
            & inputElementConfig_initialValue .~ maybe missingText (T.pack . show) initialValueActual
            & inputElementConfig_setValue .~
                leftmost [
                    T.pack . show <$> changeSelection,
                    maybe missingText (T.pack . show) <$> itemsUpdated]

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

    let itemsUpdated = updated $ headMay <$> itemsDyn

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
        (leftmost [Just <$> changeSelection, itemsUpdated])

    pure dynResult

selectOptgroups :: (Eq a, Reflex t, MonadHold t m, MonadWidget t m, Show a) =>
     T.Text
  -> T.Text
  -> Dynamic t [(T.Text, [a])]
  -> a -> m (Dynamic t (Maybe a))
selectOptgroups label missingText itemsDyn initialValue = elClass "div" "input-field" $ mdo
    initialItems <- sample $ current itemsDyn

    let initialValueActual = if initialValue `elem` (snd =<< initialItems)
        then Just initialValue
        else headMay . snd =<< headMay initialItems

    (form, changeSelection) <- elClass "div" "select-wrapper" $ do
        (form, _) <- el' "div" $ inputElement $ def
            & inputElementConfig_initialValue .~ maybe missingText (T.pack . show) initialValueActual
            & inputElementConfig_setValue .~
                leftmost [
                    T.pack . show <$> changeSelection,
                    maybe missingText (T.pack . show) <$> itemsUpdated]

        let optgroupItemsDyn = itemsDyn <&> (\x ->
                toOptgroupValues x)

        changeSelection <- fmapMaybe id <$> (elDynAttr "ul" selectAttrs $ do
            itemEvents <- dyn $ optgroupItemsDyn <&> \items -> leftmost <$> forM items (\item -> 
                case item of
                    Heading headingText -> do
                        elClass "li" "optgroup" $
                            (Nothing <$) . domEvent Click . fst <$> el' "span"
                                (text headingText)
                    OptgroupValue x -> do
                        elClass "li" "optgroup-option" $
                            ((Just x) <$) . domEvent Click . fst <$> el' "span" (
                                text $ T.pack $ show x))
            switchHold never itemEvents)

        elSvg "svg" ("class" =: "caret" <>
            "height" =: "24" <>
            "viewBox" =: "0 0 24 24" <>
            "width" =: "24" <>
            "xmlns" =: "http://www.w3.org/2000/svg") $ do
                elSvg "path" ("d" =: "M7 10l5 5 5-5z") $ pure ()
                elSvg "path" ("d" =: "M0 0h24v24H0z" <> "fill" =: "none") $ pure ()

        pure (form, changeSelection)

    elAttr "label" ("style" =: "left: 0rem;") $ text label

    let itemsUpdated = updated $ (\x -> headMay . snd =<< headMay x) <$> itemsDyn

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
        (leftmost [Just <$> changeSelection, itemsUpdated])

    pure dynResult

data OptgroupValue a =
      Heading T.Text
    | OptgroupValue a

toOptgroupValues :: [(T.Text, [a])] -> [OptgroupValue a]
toOptgroupValues [] = []
toOptgroupValues ((label, values) : xs) = 
    Heading label : fmap OptgroupValue values 
        ++ toOptgroupValues xs

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

elSvg tag a1 a2 = do
  elDynAttrNS' (Just "http://www.w3.org/2000/svg") tag (constDyn a1) a2
  return ()

liftFrontend :: (MonadSample t m, Prerender t m) => b -> IO b -> m b
liftFrontend d x = do
    res <- current <$> prerender (pure d) (liftIO x)
    sample res

liftFrontend' :: (MonadSample t m, Prerender t m) => b -> Client m b -> m b
liftFrontend' d x = do
    res <- current <$> prerender (pure d) x
    sample res

checkbox :: _ => T.Text -> Bool -> m (Dynamic t Bool)
checkbox label initialValue = do
    el "form" $ el "p" $ el "label" $ do
        res <- _checkbox_value <$> Reflex.Dom.Core.checkbox initialValue def
        elClass "span" "checkbox" $ text label
        return res

modalHeader :: _ => T.Text -> m ()
modalHeader txt = do
    elAttr "h4" ("style" =: "margin-top: 0em; padding-bottom:30px;") $ text txt
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


-- | Helper function to open a simple Ok/Cancel modal dialog that takes a validated result.
-- "Ok" can only be pressed if the passed dynamic returns a successful result.
validatedModal :: (Reflex t, MonadFix m, PostBuild t m, MonadHold t m, MonadWidget t m, DomBuilder t m)
      => Event t () -> m (Dynamic t (Validation (NonEmpty ErrorMessage) a)) -> m (Event t (Maybe a))
validatedModal onClick contents = mdo
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

    let isSuccessfulResult = current $
            res <&> \case
                Success _ -> True
                Failure _ -> False

    let onSuccessfulSubmit = gate 
            isSuccessfulResult 
            onSubmit 

    let events = leftmost
            [
                Open   <$ onClick,
                Closed <$ onCancel,
                Closed <$ onSuccessfulSubmit
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

    lastSuccessfulRes <- current <$> foldDyn 
        (\x y ->
            case x of  
                Success r -> Just r
                Failure _ -> y
        )
        Nothing (updated res)

    pure $ leftmost
      [
        Just <$> (fmapMaybe id $ tag lastSuccessfulRes onSuccessfulSubmit)
      , Nothing <$ onCancel
      ]

data ModalEvent =
      Open
    | Closed

toast message = do
    liftJSM $ eval ("console.log(\"toast\"); M.toast({html: '" <> message <> "'})" :: T.Text)
    pure ()

toastOnErrors :: (Show a, MonadSample t m, Prerender t m) => m (Either a b) -> m ()
toastOnErrors x = do
    res <- x
    case res of
        Left  e ->
            liftFrontend' () $
                toast $ "An exception occured when loading Bedelibry: " <> T.pack (show e)
        Right _ ->
            pure ()