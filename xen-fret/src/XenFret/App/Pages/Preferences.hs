
module XenFret.App.Pages.Preferences where

import qualified Data.Text as T
import Language.Javascript.JSaddle (liftJSM, jsg, jsg0, jsg3, jsg1, fromJSVal, fun, valToStr, JSString(..), textFromJSString)
import Reflex.Dom.Core hiding(Home, button, checkbox)
import Reflex.Dom.Extras
import XenFret.AppData
import XenFret.Data
import XenFret.App.Util
import Data.Aeson (encode, decode, encodeFile)
import Data.Functor
import Data.ByteString.Lazy (toStrict)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Control.Monad.IO.Class
import Control.Monad.Fix
import qualified Data.Text.Lazy.Encoding as TL
import Data.Text.Lazy (fromStrict)
import XenFret.App.Widgets.ColorPicker

preferencePage :: _ => FilePath -> m ()
preferencePage appDir = mdo
    initialAppData <- loadAppData (appDir <> "/app_data.json")
    let initialPrefs = _preferences initialAppData

    rootNoteColorDyn <- holdDyn 
        (rootNoteColor $ _preferences initialAppData)
        rootNoteColorSet

    fretboardColorDyn <- holdDyn 
        (fretboardColor $ _preferences initialAppData)
        fretboardColorSet

    -- Note: This will take a different approach from other pages,
    --  as there are multiple data types to modify.
    let prefUpdatesDyn = foldr1 (\x y -> do { x' <- x; y' <- y; pure $ x' . y' }) 
          [
            fontSizeDyn <&> \fontSize appData ->
                appData { _preferences = (_preferences appData) { noteNameSize = fontSize } }
          , rootNoteColorDyn <&> \rootNoteColor appData ->
                appData { _preferences = (_preferences appData) { rootNoteColor = rootNoteColor }}
          , fretboardColorDyn <&> \fretboardColor appData ->
                appData { _preferences = (_preferences appData) { fretboardColor = fretboardColor }}
          ]

    let dynAppData = prefUpdatesDyn <*> pure initialAppData

    (_, clickImport) <- prefRow $ do
        prefHeader "Import Data"

        el "p" $ text "Import previously exported xen fret data."

        divider

    (_, clickExport) <- prefRow $ do
        prefHeader "Export Data"

        el "p" $ text "Export xen fret data to a JSON file."

        divider

    (fontSizeDyn, _) <- prefRow $ do
        prefHeader "Note Font Size"

        el "p" $ text "Set the font size used for displaying note names."

        res <- positiveIntEntry (noteNameSize initialPrefs)

        divider

        pure res

    (_, rootColorClick) <- prefRow $ do
        prefHeader "Root Note Color"

        el "p" $ text "Set the color used to display root notes of scales."

        divider

    rootNoteColorSet <- mapMaybe id <$> modal rootColorClick (do
        modalHeader "Root Note Color"
        colorPicker "root-color" (rootNoteColor initialPrefs))

    (_, dotSizeClick) <- prefRow $ do
        prefHeader "Note Dot Size"

        el "p" $ text "Set the size used for note dots used in the diagram."

        res <- positiveDoubleEntry (dotSize initialPrefs)

        divider

        pure res

    (_, fretColorClick) <- prefRow $ do
        prefHeader "Fretboard Color"

        el "p" $ text "Set the background color used for fretboards"

        divider

    fretboardColorSet <- mapMaybe id <$> modal fretColorClick (do
        modalHeader "Fretboard Color"
        colorPicker "fretboard-color" (fretboardColor initialPrefs))

    (_, fretStyleClick) <- prefRow $ do
        prefHeader "Fret Style"

        el "p" $ text "Choose the style in which to display frets."

        divider

    (_, fretThicknessClick) <- prefRow $ do
        prefHeader "Fret Style"

        el "p" $ text "Choose the thickness in which to draw frets."

        divider

    prefRow $ do
        prefHeader "Default Temperament"

        el "p" $ text "Set the default temperament to use when creating diagrams."

        divider

    prefRow $ do
        prefHeader "Default Instrument"

        el "p" $ text "Set the default instrument to use when creating diagrams."

        divider

    prefRow $ do
        prefHeader "Default Tuning"

        el "p" $ text "Set the default tuning used per instrument."

        divider

    _ <- prerender (pure never) $ performEvent $ clickExport <&> \_ -> do
        -- appData <- liftIO $ loadAppData (appDir <> "/app_data.json")

        liftJSM $ jsg3 ("download" :: T.Text) (decodeUtf8 $ toStrict $ encode initialAppData)
            ("xen_fret_data.json" :: T.Text)
            ("text/json" :: T.Text)

    _ <- prerender (pure never) $ performEvent $ clickImport <&> \_ -> do
        liftJSM $ jsg1 ("importFile" :: T.Text)
            (fun $ \_ this args -> do
                let firstArg = args !! 0
                contents <- textFromJSString <$> valToStr firstArg
                let Just appData = decode (TL.encodeUtf8 $ fromStrict contents) :: Maybe AppData
                liftIO $ encodeFile (appDir <> "/app_data.json") appData)

    persistAppData dynAppData
        (appDir <> "/app_data.json")

    blank

prefRow :: _ => m a -> m (a, Event t ())
prefRow x = do
    (e, res) <- elAttr' "div" ("class" =: "row" <> "style" =: "margin-bottom: 0; margin-left: 2em; margin-right: 2em;") x

    pure (res, domEvent Click e)

prefHeader :: _ => T.Text -> m ()
prefHeader headerText = elAttr "p" ("class" =: "unselectable" <> "style" =: "font-weight: bold;") $ text headerText

checkboxPref :: _ => T.Text -> T.Text -> Bool -> m (Dynamic t Bool)
checkboxPref header description initialValue = do
    (res, _) <- prefRow $ do
        elClass "div" "col s10" $ do
            prefHeader header
            elClass "p" "unselectable" $ text description
        elAttr "div" ("class" =: "col s2 valign-wrapper" <> "style" =: "height: 7.5em;") $
            checkbox "" initialValue

    divider

    pure res

multiSelectPref :: _ => T.Text -> T.Text -> [a] -> [a] -> m (Dynamic t [a])
multiSelectPref header description values initialValue = do
    (res, _) <- prefRow $ do
        elClass "div" "col s10" $ do
            prefHeader header
            elClass "p" "unselectable" $ text description
        elAttr "div" ("class" =: "col s2 valign-wrapper" <> "style" =: "height: 7.5em;") $
            multiSelect values initialValue

    divider

    pure res

radioPref :: (Eq a, PostBuild t m, MonadFix m, MonadHold t m, DomBuilder t m, Show a) => T.Text -> T.Text -> [a] -> a -> m (Dynamic t a)
radioPref header description values initialValue = do
    (_, onClick) <- prefRow $ elClass "div" "col s10" $ do
        prefHeader header
        elClass "p" "unselectable" $ text description

    modalDismissEvent <- modal onClick $ do
        el "h5" $ text header
        radioGroup header values initialValue

    divider

    let submitPrefEvent = mapMaybe id modalDismissEvent

    holdDyn initialValue
        submitPrefEvent