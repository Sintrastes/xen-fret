
module XenFret.App.Pages.Preferences where

import qualified Data.Text as T
import Language.Javascript.JSaddle (liftJSM, jsg, jsg0, jsg3, jsg1, fromJSVal, fun, valToStr, JSString(..))
import Data.JSString
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

preferencePage :: _ => FilePath -> m ()
preferencePage appDir = do
    currentAppData <- loadAppData (appDir <> "/app_data.json")
    let currentPrefs = _preferences currentAppData

    (_, clickImport) <- prefRow $ do
        prefHeader "Import Data"

        el "p" $ text "Import previously exported xen fret data."

        divider

    (_, clickExport) <- prefRow $ do
        prefHeader "Export Data"

        el "p" $ text "Export xen fret data to a JSON file."

        divider

    prefRow $ do
        prefHeader "Note Font Size"

        el "p" $ text "Set the font size used for displaying note names."

        divider

    prefRow $ do
        prefHeader "Root Note Color"

        el "p" $ text "Set the color used to display root notes of scales."

        divider

    prefRow $ do
        prefHeader "Note Dot Size"

        el "p" $ text "Set the size used for note dots used in the diagram."

        divider

    prefRow $ do
        prefHeader "Fretboard Color"

        el "p" $ text "Set the background color used for fretboards"

        divider

    _ <- prerender (pure never) $ performEvent $ clickExport <&> \_ -> do
        -- appData <- liftIO $ loadAppData (appDir <> "/app_data.json")

        liftJSM $ jsg3 ("download" :: T.Text) (decodeUtf8 $ toStrict $ encode currentAppData)
            ("xen_fret_data.json" :: T.Text)
            ("text/json" :: T.Text)

    _ <- prerender (pure never) $ performEvent $ clickImport <&> \_ -> do
        liftJSM $ jsg1 ("importFile" :: T.Text)
            (fun $ \_ this args -> do
                let firstArg = args !! 0
                JSString contents <- valToStr firstArg
                let Just appData = decode (TL.encodeUtf8 $ fromStrict contents) :: Maybe AppData
                liftIO $ encodeFile (appDir <> "/app_data.json") appData)

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