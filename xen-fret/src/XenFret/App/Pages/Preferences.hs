
module XenFret.App.Pages.Preferences where

import qualified Data.Text as T
import Language.Javascript.JSaddle (liftJSM, jsg, jsg0, jsg3, jsg1, fromJSVal)
import Reflex.Dom.Core hiding(Home, button, checkbox)
import Reflex.Dom.Extras
import XenFret.AppData
import XenFret.Data
import XenFret.App.Util
import Data.Aeson (encode)
import Data.Functor
import Data.ByteString.Lazy (toStrict)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Control.Monad.IO.Class
import Control.Monad.Fix

preferencePage :: _ => FilePath -> m ()
preferencePage appDir = do
    currentAppData <- loadAppData (appDir <> "/app_data.json")
    let currentPrefs = preferences currentAppData
    el "p" $ text "Preferences"

    clickImport <- button "Import Data"

    clickExport <- button "Export Data"

    _ <- prerender (pure never) $ performEvent $ clickExport <&> \_ -> do
        -- appData <- liftIO $ loadAppData (appDir <> "/app_data.json")

        liftJSM $ jsg3 ("download" :: T.Text) (decodeUtf8 $ toStrict $ encode currentAppData)
            ("xen_fret_data.json" :: T.Text)
            ("text/json" :: T.Text)

    _ <- prerender (pure never) $ performEvent $ clickImport <&> \_ -> do
        -- TODO: Need a file chooser tool.
        pure ()

    blank

prefRow :: _ => m a -> m (a, Event t ())
prefRow x = do
    (e, res) <- elAttr' "div" ("class" =: "row" <> "style" =: "margin-bottom: 0;") x

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