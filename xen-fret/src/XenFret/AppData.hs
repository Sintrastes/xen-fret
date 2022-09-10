
module XenFret.AppData where

import Data.Aeson.TH
import XenFret.Data
import qualified Data.Text as T
import Data.Map hiding(fromList)
import Data.MultiMap
import Data.List.NonEmpty  hiding(fromList)
import Data.Ratio
import XenFret.Sagittal
    ( sagittal5CommaUp, sagittalSharp, sagittalSharp5CDown )
import Control.Lens.TH
import XenFret.App.Widgets.ColorPicker (Color(..))
import Data.Tree.Lens (root)
import Control.Lens.Internal.Fold (NonEmptyDList(NonEmptyDList))
import qualified Data.Map as Map

data LineStyle =
      Solid
    | Dashed

$(deriveJSON defaultOptions ''LineStyle)

type TemperamentName = String
type InstrumentName = String
type TuningName = String

data PreferenceData = PreferenceData {
    useDarkMode :: Bool,
    noteNameSize :: Int,
    dotSize :: Double,
    rootNoteColor :: Color,
    fretboardColor :: Color,
    fretStyle :: LineStyle,
    fretThickness :: Double,
    defaultTemperament :: Maybe TemperamentName,
    defaultInstrument :: Maybe InstrumentName,
    defaultTuning :: Map (TemperamentName, InstrumentName) TuningName
}

defaultPreferences :: PreferenceData
defaultPreferences = PreferenceData {
    useDarkMode = False,
    noteNameSize = 12,
    dotSize = 1.0,
    rootNoteColor = Color 51 92 255,
    fretboardColor = Color 255 255 255,
    fretStyle = Solid,
    fretThickness = 1.0,
    defaultTemperament = Nothing,
    defaultInstrument = Nothing,
    defaultTuning = Map.fromList []
}

$(deriveJSON defaultOptions ''PreferenceData)

data AppData = AppData {
    -- | Get the list of temperaments
    _temperaments :: [Temperament],
    -- | Get the current preferences for the app.
    _preferences :: PreferenceData
}

$(makeLenses ''AppData)

$(deriveJSON defaultOptions ''AppData)

defaultAppData :: AppData
defaultAppData = AppData {
      _temperaments =
        [
            Temperament {
                temperamentName = "11-TET"
              , divisions = 11
              , period = 2 % 1
              , notationSystems =
                  [
                    NotationSystem "" [
                      "Q","Q#","R","R#","S",
                      "S#","T","T#","U","U#","P"
                    ]
                  ]
              , chords =
                  [
                    Chord "Major" (4 :| [3, 4])
                  , Chord "Minor" (3 :| [4, 4])
                  ]
              , scales =
                  [
                    Scale "Orgone[7]"
                        (1 :| [2, 1, 2, 1, 2, 2])
                  , Scale "Machine[5]"
                        (2 :| [2, 2, 2, 3])
                  , Scale "Machine[6]"
                        (2 :| [2, 2, 2, 2, 1])
                  , Scale "Joan heptatonic"
                        (1 :| [1, 1, 3, 1, 1, 3])
                  , Scale "Joan pentatonic"
                        (1 :| [4, 1, 4, 1])
                  ]
              , tunings =
                  [
                    Tuning "Wide Fourths Tuning" "Six-String Guitar"
                        (0 :| [5, 10, 15, 20, 25]) 0
                  , Tuning "Major Thirds Tuning" "Six-String Guitar"
                        (0 :| [4, 8, 12, 16, 20]) 0
                  , Tuning "Wide Fourths Tuning" "Four-String Bass Guitar"
                        (0 :| [5, 10, 15]) 0
                  , Tuning "Major Thirds Tuning" "Four-String Bass Guitar"
                        (0 :| [4, 8, 120]) 0
                  ]
            },
            Temperament {
              temperamentName = "12-TET"
            , divisions = 12
            , period = 2 % 1
            , notationSystems =
                  [
                    NotationSystem "" [
                      "A","A#","B","C","C#","D",
                      "D#","E","F","F#","G","G#"
                    ]
                  ]
            , chords =
                  [
                    Chord "Major" (4 :| [3, 5])
                  , Chord "Minor" (3 :| [4, 5])
                  , Chord "Major 7th" (4 :| [3, 4, 1])
                  , Chord "Dominant 7th" (4 :| [3, 3, 2])
                  , Chord "Minor 7th" (3 :| [4, 3, 2])
                  , Chord "MinMaj 7th" (3 :| [4, 4, 1])
                  ]
            , scales =
                  [
                    Scale "Ionian (Major)"
                        (2 :| [2, 1, 2, 2, 2, 1])
                  , Scale "Mixolydian"
                        (2 :| [2, 1, 2, 2, 1, 2])
                  , Scale "Minor"
                        (2 :| [1, 2, 2, 1, 2, 2])
                  , Scale "Dorian"
                        (2 :| [1, 2, 2, 2, 1, 2])
                  , Scale "diminished[8] (Octatonic)"
                        (2 :| [1, 2, 1, 2, 1, 2, 1])
                  , Scale "Whole tone"
                        (2 :| [2, 2, 2, 2, 2])
                  , Scale "augmented[6]"
                        (3 :| [1, 3, 1, 3, 1])
                  , Scale "Blues"
                        (3 :| [2, 1, 1, 3, 2])
                  , Scale "Mixolydian b6"
                        (2 :| [2, 1, 2, 1, 2, 2])
                  , Scale "Hirojoshi"
                        (2 :| [1, 4, 1, 4])
                  , Scale "Ryo"
                        (2 :| [2, 3, 2, 3])
                  , Scale "Insen"
                        (1 :| [4, 2, 3, 2])
                  , Scale "Engimatic Scale"
                        (1 :| [3, 2, 2, 2, 1, 1])
                  ]
            , tunings =
                  [
                    Tuning "Standard Tuning" "Mandolin"
                        (fmap (+10) $ 0 :| [7, 14, 21]) 0
                  , Tuning "Standard Tuning" "Ukulele"
                        (fmap (+3) $ 7 :| [0, 4, 9]) 0
                  , Tuning "Standard Tuning" "Six-String Guitar"
                        (fmap (+7) $ 0 :| [5, 10, 15, 19, 24]) 0
                  , Tuning "Standard Tuning" "Four-String Bass Guitar"
                        (fmap (+7) $ 0 :| [5, 10, 15]) 0
                  , Tuning "Standard Tuning" "Seven-String Guitar"
                        (fmap (+2) $ 0 :| [5, 10, 15, 20, 14, 29]) 0
                  , Tuning "Drop D" "Six-String Guitar"
                        (fmap (+5) $ 0 :| [7, 12, 17, 21, 26]) 0
                  , Tuning "DADGAD" "Six-String Guitar"
                        (fmap (+5) $ 0 :| [7, 12, 17, 19, 24]) 0
                  , Tuning "All Fourths" "Six-String Guitar"
                        (fmap (+7) $ 0 :| [5, 10, 15, 20, 25]) 0
                  , Tuning "All Fifths" "Six-String Guitar"
                        (0 :| [7, 14, 21, 28, 35]) 0
                  ]
            },
            Temperament {
              temperamentName = "13-TET"
            , divisions = 13
            , period = 2 % 1
            , notationSystems =
                  [
                    NotationSystem "" [
                       "J","J#","K","L","L#","M","M#",
                       "N","O","O#","P","Q","Q#"
                    ]
                  ]
            , chords =
                  [

                  ]
            , scales =
                  [
                    Scale "Archeotonic (Ryonian Mode)"
                        (2 :| [2, 2, 2, 2, 2, 1])
                  , Scale "Oneirotonic (Dylathian Mode)"
                        (2 :| [2, 1, 2, 2, 1, 2, 1])
                  ]
            , tunings =
                  [
                    Tuning "Oneirotonic Tuning" "Six-String Guitar"
                        (3 :| [8, 14, 19, 24, 29]) 0
                  ]
            },
            Temperament {
              temperamentName = "14-TET"
            , divisions = 14
            , period = 2 % 1
            , notationSystems =
                  [
                    NotationSystem "" [
                     "A","^A","B","^B","C","^C","D","^D","E","^E",
                     "F","^F","G","^G"
                    ]
                  ]
            , chords =
                  [

                  ]
            , scales =
                  [
                   Scale "Titanium[9]"
                        (2 :| [1, 2, 1, 2, 1, 2, 1, 2])
                  ]
            , tunings =
                  [
                    Tuning "Wide Fourths Tuning" "Six-String Guitar"
                        (0 :| [5, 10, 15, 20, 25]) 0
                  ]
            },
            Temperament {
              temperamentName = "15-TET"
            , divisions = 15
            , period = (2 % 1)
            , notationSystems =
                  [
                    NotationSystem "" [
                      "α","β\\","β","χ\\","χ","δ\\","δ",
                      "ε\\","ε","φ\\","φ","γ\\","γ","η\\",
                      "η"
                    ]
                  ]
            , chords =
                  [

                  ]
            , scales =
                  [
                    Scale "Augmented[6]"
                        (4 :| [1, 4, 1, 4, 1])
                  , Scale "Triforce[6]"
                        (3 :| [2, 3, 2, 3, 2])
                  , Scale "Porcupine[7]"
                        (3 :| [2, 2, 2, 2, 2, 2])
                  , Scale "Orgone[7]"
                        (1 :| [3, 1, 3, 1, 3, 3])
                  , Scale "Porcupine[8]"
                        (2 :| [1, 2, 2, 2, 2, 2, 2])
                  , Scale "Augmented[9]"
                        (3 :| [1, 1, 3, 1, 1, 3, 1, 1])
                  , Scale "Triforce[9]"
                        (2 :| [1, 2, 2, 1, 2, 2, 1, 2])
                  , Scale "Blackwood[10]"
                        (2 :| [1, 2, 1, 2, 1, 2, 1, 2, 1])
                  , Scale "Marvel double harmonic major"
                        (1 :| [4,1,3,1,4,1])
                  ]
            , tunings =
                  [
                    Tuning "All Fourths Tuning" "Six-String Guitar"
                        (0 :| [5, 10, 15, 20, 25]) 0
                  ]
            },
            Temperament {
              temperamentName = "16-TET"
            , divisions = 16
            , period = 2 % 1
            , notationSystems =
                  [
                    NotationSystem "Standard" [
                      "A","B#","B","Bb","C#","C","D#","D",
                      "E#","E","Eb","F#","F","G#","G","A#"
                    ]
                  ]
            , chords =
                  [

                  ]
            , scales =
                  [
                    Scale "mavila[7]"
                        (2 :| [2, 2, 3, 2, 2, 3])
                  , Scale "Lemba"
                        (3 :| [3, 2, 3, 3, 2])
                  , Scale "Magic"
                        (1 :| [4, 1, 4, 1, 4, 1])
                  ]
            , tunings =
                  [
                    Tuning "Wide Fourths Tuning" "Six-String Guitar"
                        (fmap (+9) $ 0 :| [7, 14, 21, 28, 35]) 0
                  , Tuning "Diminished Fourths Tuning" "Six-String Guitar"
                        (fmap (+9) $ 0 :| [6, 12, 18, 24, 30]) 0
                  , Tuning "Wide Fourths Tuning (7 String)" "Seven-String Guitar"
                        (fmap (+9) $ 0 :| [7, 14, 21, 28, 35, 40]) 0
                  , Tuning "Diminished Fourths Tuning (7 String)" "Seven-String Guitar"
                        (fmap (+2) $ 0 :| [6, 12, 18, 24, 30, 36]) 0
                  ]
            },
            Temperament {
              temperamentName = "17-TET"
            , divisions = 17
            , period = 2 % 1
            , notationSystems =
                  [
                    NotationSystem "Standard" [
                      "A","Bb","A#","B","C","Db","C#","D",
                      "Eb","D#","E","F","Gb","F#","G","Ab",
                      "G#"
                    ]
                  ]
            , chords =
                  [

                  ]
            , scales =
                  [
                    Scale "Major"
                        (3 :| [3, 3, 1, 3, 3, 1])
                  , Scale "Maqamic[7]"
                        (3 :| [2, 3, 2, 3, 2, 2])
                  , Scale "Squares[8]"
                        (1 :| [1, 4, 1, 4, 1, 4])
                  , Scale "Hydra"
                        (3 :| [3, 1, 1, 2, 3, 2, 1, 1])
                  , Scale "Springfieldian"
                        (3 :| [3, 2, 2, 3, 3, 1])
                  , Scale "Northhaverbrookian"
                        (2 :| [3, 3, 1, 3, 3, 2])
                  , Scale "Shelbyvillean"
                        (3 :| [3, 1, 3, 3, 2, 2])
                  ]
            , tunings =
                  [
                    Tuning "Standard Tuning" "Six-String Guitar"
                        (fmap (+10) $ 0 :| [7, 14, 21, 27, 34]) 0
                  , Tuning "All Fourths" "Six-String Guitar"
                        (fmap (+10) $ 0 :| [7, 14, 21, 28, 35]) 0
                  ]
            },
            Temperament {
              temperamentName = "18-TET"
            , divisions = 18
            , period = 2 % 1
            , notationSystems =
                  [
                    NotationSystem "" [
                        "A","Bb","A#","B","C","Db","C#","D",
                        "Eb","D#","E","F","Gb","F#","G",
                        "Hb","G#","H"
                    ]
                  ]
            , chords =
                  [

                  ]
            , scales =
                  [
                    Scale "Bicycle"
                        (4 :| [4, 1, 4, 4, 1])
                  , Scale "Mish Heptatonic"
                        (3 :| [2, 3, 2, 3, 3, 2])
                  ]
            , tunings =
                  [
                    Tuning "Wide Fourths" "Six-String Guitar"
                        (0 :| [8, 16, 24, 32, 40]) 0
                  ]
            },
            Temperament {
              temperamentName = "19-TET"
            , divisions = 19
            , period = 2 % 1
            , notationSystems =
                  [
                    NotationSystem "Standard" [
                      "A","A#","Bb","B","B#","C",
                      "C#","Db","D","D#","Eb","E",
                      "E#","F","F#","Gb","G","G#","Ab"
                    ]
                  ]
            , chords =
                  [

                  ]
            , scales =
                  [
                    Scale "Ionian (Major)"
                        (3 :| [3, 2, 3, 3, 3, 2])
                  , Scale "Sensi[8]"
                        (2 :| [3, 2, 2, 3, 2, 2, 3])
                  , Scale "Negri[9]"
                        (2 :| [2, 2, 2, 3, 2, 2, 2, 2])
                  , Scale "Kleismic[7]"
                        (1 :| [4, 1, 4, 1, 4, 4])
                  ]
            , tunings =
                  [
                    Tuning "Standard Tuning" "Six-String Guitar"
                        (fmap (+11) $ 0 :| [8, 16, 24, 30, 38]) 0
                  ]
            },
            Temperament {
              temperamentName = "22-TET"
            , divisions = 22
            , period = 2 % 1
            , notationSystems =
                  [
                    NotationSystem "Sagittal" [
                      "A",
                      "A" <> sagittal5CommaUp,
                      "A" <> sagittalSharp5CDown,
                      "A" <> sagittalSharp,
                      "B",
                      "C",
                      "C" <> sagittal5CommaUp,
                      "C" <> sagittalSharp5CDown,
                      "C" <> sagittalSharp,
                      "D",
                      "D" <> sagittal5CommaUp,
                      "D" <> sagittalSharp5CDown,
                      "D" <> sagittalSharp,
                      "E",
                      "F",
                      "F" <> sagittal5CommaUp,
                      "F" <> sagittalSharp5CDown,
                      "F" <> sagittalSharp,
                      "G",
                      "G" <> sagittal5CommaUp,
                      "G" <> sagittalSharp5CDown,
                      "G" <> sagittalSharp
                    ]
                  , NotationSystem "Standard (Meantone)" [
                      "A","A#","Bb","B","B#",
                      "Cb","C","C#","Db","D",
                      "D#","Eb","E","E#","Fb",
                      "F","F#","Gb","G","G#",
                      "Gx","Ab"
                    ]
                  ]
            , chords =
                  [

                  ]
            , scales =
                  [
                    Scale "Superpyth[7] (Major)"
                        (4 :| [4, 1, 4, 4, 4, 1])
                  , Scale "Astrology[6]"
                        (4 :| [3, 4, 4, 3, 4])
                  , Scale "Porcupine[7]"
                        (3 :| [3, 3, 4, 3, 3, 3])
                  , Scale "Pajara[10]"
                        (2 :| [2, 3, 2, 2, 2, 2, 3, 2, 2])
                  ]
            , tunings =
                  [
                    Tuning "Standard Tuning (22-TET)" "Six-String Guitar"
                        (fmap (+13) $ 0 :| [9, 18, 27, 35, 44]) 0
                  ]
            },
            Temperament {
              temperamentName = "24-TET"
            , divisions = 24
            , period = 2 % 1
            , notationSystems =
                  [

                  ]
            , chords =
                  [

                  ]
            , scales =
                  [
                    Scale "Ionian (Major)"
                        (4 :| [4, 2, 4, 4, 4, 2])
                  , Scale "Anchihoye: Ethiopia"
                        (2 :| [8, 3, 6, 5])
                  , Scale "Enharmonic Phrygian"
                        (8 :| [1, 1, 8, 4, 1, 1])
                  , Scale "Maqam Rast"
                        (4 :| [3, 3, 4, 4, 3, 3])
                  , Scale "Mohajira[7]"
                        (3 :| [4, 3, 4, 3, 4, 3])
                  ]
            , tunings =
                  [
                    Tuning "Standard Tuning" "Six-String Guitar"
                        (fmap (+14) $ 0 :| [10, 20, 30, 38, 48]) 0
                  , Tuning "Drop D" "Six-String Guitar"
                        (fmap (+12) $ 0 :| [14, 24, 34, 42, 52]) 0
                  ]
            },
            Temperament {
              temperamentName = "31-TET"
            , divisions = 31
            , period = 2 % 1
            , notationSystems =
                  [

                  ]
            , chords =
                  [

                  ]
            , scales =
                  [

                  ]
            , tunings =
                  [

                  ]
            },
            Temperament {
              temperamentName = "41-TET"
            , divisions = 41
            , period = 2 % 1
            , notationSystems =
                  [

                  ]
            , chords =
                  [

                  ]
            , scales =
                  [
                    Scale "Down Lydian"
                        (7 :| [6,7,4,7,6,4])
                  , Scale "Down Major"
                        (7 :| [6, 4, 7, 6, 7, 4])
                  , Scale "Down Mixolydian"
                        (6 :| [7, 4, 7, 6, 4, 7])
                  , Scale "Up Minor"
                        (7 :| [4, 6, 7, 4, 7, 6])
                  , Scale "Up Phrygian"
                        (4 :| [7, 6, 7, 4, 6, 7])
                  , Scale "Up Dorian"
                        (7 :| [4, 6, 7, 7, 4, 6])
                  , Scale "Up Locrian"
                        (4 :| [6, 7, 3, 8, 6, 7])
                  , Scale "Up Lydian"
                        (7 :| [8, 7, 2, 7, 8, 2])
                  , Scale "Up Major"
                        (7 :| [8, 2, 7, 8, 7, 2])
                  , Scale "Up Mixolydian"
                        (8 :| [7, 2, 7, 8, 2, 7])
                  , Scale "Down Minor"
                        (7 :| [2, 8, 7, 2, 7, 8])
                  , Scale "Down Phrygian"
                        (2 :| [7, 8, 7, 2, 8, 7])
                  , Scale "Down Dorian"
                        (7 :| [2, 8, 7, 7, 2, 8])
                  , Scale "Down Locrian"
                        (2 :| [8, 7, 3, 6, 8, 7])
                  ]
            , tunings =
                  [
                    Tuning "Standard Tuning" "Kite Guitar"
                        (0 :| [13, 26, 39, 52, 65]) 1
                  ]
            },
            Temperament {
              temperamentName = "Bohlen Pierce"
            , divisions = 13
            , period = 3 % 1
            , notationSystems =
                  [
                    NotationSystem "Standard" [
                      "A","A#","B","C","C#","D","E","F","F#","G","H","H#","J"
                    ]
                  ]
            , chords =
                  [

                  ]
            , scales =
                  [
                    Scale "Lambda"
                        (2 :| [1, 1, 2, 1, 2, 1, 2, 1])
                  , Scale "Moll 1"
                        (1 :| [2,1,2,1,2,1,2,1])
                  , Scale "Harmonic"
                        (1 :| [2,1,2,1,2,1,1,2])
                  , Scale "Dur I"
                        (1 :| [2,1,2,1,1,2,1,2])
                  , Scale "Moll 2"
                        (2 :| [1,2,1,1,2,1,2,1])
                  , Scale "Dur II"
                        (2 :| [1,1,2,1,2,1,1,2])
                  , Scale "Gamma"
                        (1 :| [2,1,2,1,1,2,2,1])
                  , Scale "Walker A"
                        (1 :| [1,2,1,2,1,2,1,2])
                  , Scale "Walker B"
                        (1 :| [2,1,1,2,1,2,1,2])
                  , Scale "Walker I"
                        (2 :| [1,2,1,2,1,2,1,1])
                  , Scale "Walker II"
                        (2 :| [1,2,1,2,1,1,2,1])
                  ]
            , tunings =
                  [
                    Tuning "Bohlen's Tuning" "Six String Guitar"
                        (0 :| [3,6,9,13,16]) 0
                  ]
            }
        ]
    , _preferences = defaultPreferences
}