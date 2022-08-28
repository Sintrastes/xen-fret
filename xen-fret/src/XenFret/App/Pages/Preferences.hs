
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

    blank