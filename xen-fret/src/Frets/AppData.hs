
module Frets.AppData where

import Data.Aeson.TH
import Frets.Data
import qualified Data.Text as T
import Data.Map hiding(fromList)
import Data.MultiMap
import Data.List.NonEmpty  hiding(fromList)
import Data.Ratio

data PreferenceData = PreferenceData {
    useDarkMode :: Bool
}

$(deriveJSON defaultOptions ''PreferenceData)

data AppData = AppData {
    -- | Get the list of temperaments
    temperaments :: [Temperament],
    -- | Get the tunings associated with a temperament.
    tunings :: Map T.Text [Tuning],
    -- | Get the scales associated with a temperament.
    scales  :: Map T.Text [Scale],
    preferences :: PreferenceData
}

$(deriveJSON defaultOptions ''AppData)

defaultAppData = AppData {
      temperaments = 
        [
            Temperament "11-TET" 11 (2 % 1),
            Temperament "12-TET" 12 (2 % 1),
            Temperament "13-TET" 13 (2 % 1),
            Temperament "16-TET" 16 (2 % 1),
            Temperament "19-TET" 19 (2 % 1),
            Temperament "22-TET" 22 (2 % 1),
            Temperament "24-TET" 24 (2 % 1),
            Temperament "Bohlen Pierce" 13 (3 % 1)
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
                (2 :| [1,2,2,2,1,2]))
          , ("13-TET", Scale "Archeotonic (Ryonian Mode)"
                (2 :| [2,2,2,2,2,1]))
          , ("13-TET", Scale "Oneirotonic (Dylathian Mode)"
                (2 :| [2,1,2,2,1,2,1]))
          , ("16-TET", Scale "mavila[7]"
                (2 :| [2,2,3,2,2,3]))
          , ("16-TET", Scale "Lemba"
                (3 :| [3,2,3,3,2]))
          , ("16-TET", Scale "Magic"
                (1 :| [4,1,4,1,4,1]))
          , ("19-TET", Scale "Ionian (Major)"
                (3 :| [3,2,3,3,3,2]))
          , ("19-TET", Scale "Sensi[8]"
                (2 :| [3,2,2,3,2,2,3]))
          , ("19-TET", Scale "Negri[9]"
                (2 :| [2,2,2,3,2,2,2,2]))
          , ("19-TET", Scale "Kleismic[7]"
                (1 :| [4,1,4,1,4,4]))
          , ("22-TET", Scale "Superpyth[7] (Major)"
                (4 :| [4,1,4,4,4,1]))
          , ("22-TET", Scale "Astrology[6]"
                (4 :| [3,4,4,3,4]))
          , ("22-TET", Scale "Porcupine[7]"
                (3 :| [3,3,4,3,3,3]))
          , ("22-TET", Scale "Pajara[10]"
                (2 :| [2,3,2,2,2,2,3,2,2]))
          , ("24-TET", Scale "Ionian (Major)"
                (4 :| [4,2,4,4,4,2]))
          , ("24-TET", Scale "Anchihoye: Ethiopia"
                (2 :| [8, 3, 6, 5]))
          , ("24-TET", Scale "Enharmonic Phrygian"
                (8 :| [1, 1, 8, 4, 1, 1]))
          , ("24-TET", Scale "Maqam Rast"
                (4 :| [3, 3, 4, 4, 3, 3]))
          , ("24-TET", Scale "Mohajira[7]"
                (3 :| [4,3,4,3,4,3]))
          , ("Bohlen Pierce", Scale "Lambda"
                (2 :| [1,1,2,1,2,1,2,1]))
        ]
    , preferences = defaultPreferences
}

defaultPreferences = PreferenceData {
    useDarkMode = False
}