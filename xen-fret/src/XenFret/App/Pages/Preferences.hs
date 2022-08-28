
module XenFret.App.Pages.Preferences where

import Reflex.Dom.Core hiding(Home, button, checkbox)
import Reflex.Dom.Extras
import XenFret.AppData
import XenFret.Data
import XenFret.App.Util

preferencePage :: _ => FilePath -> m ()
preferencePage appDir = do
    appData <- loadAppData (appDir <> "/app_data.json")
    let currentPrefs = preferences appData
    el "p" $ text "Preferences"

    clickImport <- button "Import Data"
    
    clickExport <- button "Export Data"

    blank