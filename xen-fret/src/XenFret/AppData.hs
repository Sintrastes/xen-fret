
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
                _temperamentName = "11-TET"
              , _divisions = 11
              , _period = 2 % 1
              , _notationSystems =
                  [
                    NotationSystem "" [
                      "Q","Q#","R","R#","S",
                      "S#","T","T#","U","U#","P"
                    ]
                  ]
              , _chords =
                  [
                    Chord "Major" (4 :| [3, 4])
                  , Chord "Minor" (3 :| [4, 4])
                  ]
              , _scales =
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
              , _tunings =
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
              _temperamentName = "12-TET"
            , _divisions = 12
            , _period = 2 % 1
            , _notationSystems =
                  [
                    NotationSystem "" [
                      "A","A#","B","C","C#","D",
                      "D#","E","F","F#","G","G#"
                    ]
                  ]
            , _chords =
                  [
                    Chord "Major" (4 :| [3, 5])
                  , Chord "Minor" (3 :| [4, 5])
                  , Chord "Major 7th" (4 :| [3, 4, 1])
                  , Chord "Dominant 7th" (4 :| [3, 3, 2])
                  , Chord "Minor 7th" (3 :| [4, 3, 2])
                  , Chord "MinMaj 7th" (3 :| [4, 4, 1])
                  ]
            , _scales =
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
            , _tunings =
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
              _temperamentName = "13-TET"
            , _divisions = 13
            , _period = 2 % 1
            , _notationSystems =
                  [
                    NotationSystem "" [
                       "J","J#","K","L","L#","M","M#",
                       "N","O","O#","P","Q","Q#"
                    ]
                  ]
            , _chords =
                  [

                  ]
            , _scales =
                  [
                    Scale "Archeotonic (Ryonian Mode)"
                        (2 :| [2, 2, 2, 2, 2, 1])
                  , Scale "Oneirotonic (Dylathian Mode)"
                        (2 :| [2, 1, 2, 2, 1, 2, 1])
                  ]
            , _tunings =
                  [
                    Tuning "Oneirotonic Tuning" "Six-String Guitar"
                        (3 :| [8, 14, 19, 24, 29]) 0
                  ]
            },
            Temperament {
              _temperamentName = "14-TET"
            , _divisions = 14
            , _period = 2 % 1
            , _notationSystems =
                  [
                    NotationSystem "" [
                     "A","^A","B","^B","C","^C","D","^D","E","^E",
                     "F","^F","G","^G"
                    ]
                  ]
            , _chords =
                  [

                  ]
            , _scales =
                  [
                    Scale "Titanium[9]"
                        (2 :| [1, 2, 1, 2, 1, 2, 1, 2])
                  , Scale "antipentic"
                        (4 :| [1, 4, 1, 4])
                  , Scale "Manual"
                        (3 :| [3, 2, 3, 3])
                  , Scale "Citric"
                        (3 :| [1, 3, 3, 1, 3])
                  , Scale "Ekic"
                        (2 :| [2, 1, 2, 2, 2, 1, 2])
                  , Scale "Semiquartal"
                        (2 :| [1, 2, 1, 2, 1, 2, 1, 2])
                  ]
            , _tunings =
                  [
                    Tuning "Wide Fourths Tuning" "Six-String Guitar"
                        (0 :| [5, 10, 15, 20, 25]) 0
                  ]
            },
            Temperament {
              _temperamentName = "15-TET"
            , _divisions = 15
            , _period = (2 % 1)
            , _notationSystems =
                  [
                    NotationSystem "" [
                      "α","β\\","β","χ\\","χ","δ\\","δ",
                      "ε\\","ε","φ\\","φ","γ\\","γ","η\\",
                      "η"
                    ]
                  ]
            , _chords =
                  [

                  ]
            , _scales =
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
                  , Scale "Ptolemy diatonic, \"just\" major"
                        (3 :| [2, 1, 3, 2, 3, 1])
                  , Scale "Ptolemy diatonic, natural minor"
                        (3 :| [1, 2, 3, 1, 3, 2])
                  , Scale "tetrachordal major, Sa grama"
                        (3 :| [2, 1, 3, 3, 2, 1])
                  , Scale "tetrachordal minor"
                        (3 :| [1, 2, 3, 1, 2, 3])
                  , Scale "Porcupine bright major #7"
                        (3 :| [2, 2, 2, 2, 3, 1])
                  , Scale "Porcupine bright major #6 #7"
                        (3 :| [2, 2, 2, 3, 2, 1])
                  , Scale "Porcupine bright minor #2"
                        (3 :| [1, 3, 2, 2, 2, 2])
                  , Scale "Porcupine dark minor #2"
                        (3 :| [1, 2, 3, 2, 2, 2])
                  , Scale "Porcupine bright harmonic 11th"
                        (3 :| [2, 2, 2, 2, 1, 3])
                  ]
            , _tunings =
                  [
                    Tuning "All Fourths Tuning" "Six-String Guitar"
                        (0 :| [5, 10, 15, 20, 25]) 0
                  ]
            },
            Temperament {
              _temperamentName = "16-TET"
            , _divisions = 16
            , _period = 2 % 1
            , _notationSystems =
                  [
                    NotationSystem "Standard" [
                      "A","B#","B","Bb","C#","C","D#","D",
                      "E#","E","Eb","F#","F","G#","G","A#"
                    ]
                  ]
            , _chords =
                  [

                  ]
            , _scales =
                  [
                    Scale "Mavilla[5]"
                        (5 :| [2, 5, 2, 2])
                  , Scale "Mavila[7]"
                        (2 :| [2, 2, 3, 2, 2, 3])
                  , Scale "Mavilla[9]"
                        (1 :| [2, 2, 2, 1, 2, 2, 2, 2])
                  , Scale "Lemba[6]"
                        (3 :| [3, 2, 3, 3, 2])
                  , Scale "Lemba[10]"
                        (2 :| [1, 2, 1, 2, 2, 1, 2, 1, 2])
                  , Scale "Magic[7]"
                        (1 :| [4, 1, 4, 1, 4, 1])
                  , Scale "Magic[10]"
                        (1 :| [3, 1, 1, 3, 1, 1, 1, 3, 1])
                  , Scale "Gorgo[5]"
                        (3 :| [3, 4, 3, 3])
                  , Scale "Gorgo[6]"
                        (3 :| [3, 1, 3, 3, 3])
                  , Scale "Gorgo[11]"
                        (1 :| [2, 1, 2, 1, 2, 1, 2, 1, 2, 1])
                  , Scale "Diminished[8]"
                        (1 :| [3, 1, 3, 1, 3, 1, 3])
                  ]
            , _tunings =
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
              _temperamentName = "17-TET"
            , _divisions = 17
            , _period = 2 % 1
            , _notationSystems =
                  [
                    NotationSystem "Standard" [
                      "A","Bb","A#","B","C","Db","C#","D",
                      "Eb","D#","E","F","Gb","F#","G","Ab",
                      "G#"
                    ]
                  ]
            , _chords =
                  [

                  ]
            , _scales =
                  [
                    Scale "Major"
                        (3 :| [3, 3, 1, 3, 3, 1])
                  , Scale "Maqamic[7] (bish mode)"
                        (2 :| [3, 2, 3, 2, 3, 2])
                  , Scale "Maqamic[7] (dril mode)"
                        (3 :| [2, 3, 2, 3, 2, 2])
                  , Scale "Maqamic[7] (fish mode)"
                        (2 :| [3, 2, 3, 2, 2, 3])
                  , Scale "Maqamic[7] (gil mode)"
                        (3 :| [2, 3, 2, 2, 3, 2])
                  , Scale "Maqamic[7] (jwl mode)"
                        (2 :| [3, 2, 2, 3, 2, 3])
                  , Scale "Maqamic[7] (kleeth mode)"
                        (3 :| [2, 2, 3, 2, 3, 2])
                  , Scale "Maqamic[7] (led mode)"
                        (2 :| [2, 3, 2, 3, 2, 3])
                  , Scale "Maqamic[10]"
                        (2 :| [2, 2, 1, 2, 2, 1, 2, 2, 1])
                  , Scale "Lovecraft[9]"
                        (3 :| [1, 3, 1, 3, 1, 3, 1, 1])
                  , Scale "Squares[5]"
                        (5 :| [5, 1, 5, 1])
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
                  , Scale "Otonal 17"
                        (3 :| [2, 3, 2, 2, 2, 3])
                  , Scale "Bleu[8]"
                        (3 :| [2, 2, 2, 2, 2, 2, 2])
                  , Scale "Bleu[9]"
                        (1 :| [2, 2, 2, 2, 2, 2, 2, 2])
                  , Scale "Machine[5]"
                        (5 :| [3, 3, 3, 3])
                  , Scale "Machine[6]"
                        (2 :| [3, 3, 3, 3, 3])
                  , Scale "Machine[11]"
                        (2 :| [2, 1, 2, 1, 2, 1, 2, 1, 2, 1])
                  , Scale "Huxley[5]"
                        (1 :| [4, 4, 4, 4])
                  , Scale "Huxley[9]"
                        (1 :| [1, 3, 1, 3, 1, 3, 1, 3])
                  ]
            , _tunings =
                  [
                    Tuning "Standard Tuning" "Six-String Guitar"
                        (fmap (+10) $ 0 :| [7, 14, 21, 27, 34]) 0
                  , Tuning "All Fourths" "Six-String Guitar"
                        (fmap (+10) $ 0 :| [7, 14, 21, 28, 35]) 0
                  ]
            },
            Temperament {
              _temperamentName = "18-TET"
            , _divisions = 18
            , _period = 2 % 1
            , _notationSystems =
                  [
                    NotationSystem "" [
                        "A","Bb","A#","B","C","Db","C#","D",
                        "Eb","D#","E","F","Gb","F#","G",
                        "Hb","G#","H"
                    ]
                  ]
            , _chords =
                  [

                  ]
            , _scales =
                  [
                    Scale "Antipentic"
                        (4 :| [4, 3, 4, 3])
                  , Scale "Bicycle"
                        (4 :| [4, 1, 4, 4, 1])
                  , Scale "Mavila[5]"
                        (2 :| [6, 2, 6, 2])
                  , Scale "Malic[6]"
                        (2 :| [5, 2, 2, 5, 2])
                  , Scale "Mish Heptatonic"
                        (3 :| [2, 3, 2, 3, 3, 2])
                  , Scale "Smitonic"
                        (3 :| [2, 3, 2, 3, 3, 2])
                  , Scale "Oneirotonic"
                        (3 :| [1, 3, 3, 1, 3, 3, 1])
                  , Scale "Antiekic"
                        (2 :| [2, 3, 2, 2, 2, 3, 2])
                  , Scale "Tcherepnin"
                        (4 :| [1, 1, 4, 1, 1, 4, 1, 1])
                  , Scale "Taric"
                        (2 :| [2, 1, 2, 2, 2, 2, 1, 2, 2])
                  ]
            , _tunings =
                  [
                    Tuning "Wide Fourths" "Six-String Guitar"
                        (0 :| [8, 16, 24, 32, 40]) 0
                  ]
            },
            Temperament {
              _temperamentName = "19-TET"
            , _divisions = 19
            , _period = 2 % 1
            , _notationSystems =
                  [
                    NotationSystem "Standard" [
                      "A","A#","Bb","B","B#","C",
                      "C#","Db","D","D#","Eb","E",
                      "E#","F","F#","Gb","G","G#","Ab"
                    ]
                  ]
            , _chords =
                  [

                  ]
            , _scales =
                  [
                    Scale "Ionian (Major)"
                        (3 :| [3, 2, 3, 3, 3, 2])
                  , Scale "Sensi[5]"
                        (5 :| [5, 2, 5, 2])
                  , Scale "Sensi[8]"
                        (2 :| [3, 2, 2, 3, 2, 2, 3])
                  , Scale "Negri[9]"
                        (2 :| [2, 2, 2, 3, 2, 2, 2, 2])
                  , Scale "Negri[10]"
                        (2 :| [2, 2, 2, 2, 2, 2, 2, 2, 1])
                  , Scale "Kleismic[7]"
                        (1 :| [4, 1, 4, 1, 4, 4])
                  , Scale "Semaphore[5]"
                        (4 :| [4, 4, 4, 3])
                  , Scale "Semaphore[9]"
                        (3 :| [3, 1, 3, 1, 3, 1, 3, 1])
                  , Scale "Magic[7]"
                        (5 :| [1, 5, 1, 5, 1, 1])
                  , Scale "Magic[10]"
                        (4 :| [1, 1, 4, 1, 1, 4, 1, 1, 1])
                  , Scale "Marvel hexatonic"
                        (4 :| [2, 5, 2, 4, 2])
                  , Scale "deutone[6]"
                        (4 :| [3, 3, 3, 3, 3])
                  , Scale "deutone[7]"
                        (3 :| [3, 3, 3, 3, 3, 1])
                  , Scale "kleismic[7]"
                        (4 :| [4, 1, 4, 1, 4, 1])
                  , Scale "liese[5]"
                        (8 :| [1, 8, 1, 1])
                  , Scale "liese[7]"
                        (7 :| [1, 1, 7, 1, 1, 1])
                  , Scale "liese[9]"
                        (6 :| [1, 1, 1, 6, 1, 1, 1, 1])
                  ]
            , _tunings =
                  [
                    Tuning "Standard Tuning" "Six-String Guitar"
                        (fmap (+11) $ 0 :| [8, 16, 24, 30, 38]) 0
                  ]
            },
            Temperament {
              _temperamentName = "20-TET"
            , _divisions = 20
            , _period = 2 % 1
            , _notationSystems = []
            , _chords = []
            , _scales =
                  [
                    Scale "Blackwood Major Decatonic"
                        (3 :| [1, 3, 1, 3, 1, 3, 1, 3, 1])
                  , Scale "Blackwood Minor Decatonic"
                        (1 :| [3, 1, 3, 1, 3, 1, 3, 1, 3])
                  , Scale "Blackwood Major Pentadecatonic"
                        (2 :| [1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1])
                  , Scale "Blackwood Diminished Pentadecatonic"
                        (1 :| [1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 2])
                  , Scale "Blackwood Minor Pentadecatonic"
                        (1 :| [2, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1])
                  , Scale "Balzano Nine-tone"
                        (2 :| [3, 2, 2, 2, 3, 2, 2, 2])
                  , Scale "Balzano Eleven-tone"
                        (2 :| [2, 2, 2, 1, 2, 2, 2, 2, 2, 1])
                  , Scale "Balzano Nine-tone inverse"
                        (2 :| [2, 2, 3, 2, 2, 2, 3, 2])
                  , Scale "Balzano Eleven-tone inverse"
                        (1 :| [2, 2, 2, 2, 2, 1, 2, 2, 2, 2])
                  , Scale "Octatonic"
                        (2 :| [3, 2, 3, 2, 3, 2, 3])
                  , Scale "Diminished"
                        (3 :| [2, 3, 2, 3, 2, 3, 2])
                  , Scale "Dodecatonic"
                        (2 :| [2, 1, 2, 2, 1, 2, 2, 1, 2, 2, 1])
                  , Scale "Major"
                        (4 :| [3, 1, 4, 3, 4, 1])
                  , Scale "Minor"
                        (4 :| [1, 3, 4, 1, 4, 3])
                  , Scale "Major quasi-equal Heptatonic" 
                        (3 :| [3, 3, 3, 3, 3, 2])
                  , Scale "Minor quasi-equal Heptatonic"
                        (3 :| [2, 3, 3, 3, 3, 3])
                  , Scale "Rothenberg Generalized Diatonic"
                        (3 :| [2, 2, 2, 2, 3, 2, 2, 2])
                  , Scale "Stearns Major"
                        (3 :| [4, 1, 4, 3, 3, 2])
                  , Scale "score5"
                        (7 :| [2, 7, 2, 2])
                  , Scale "Mavilla[7]"
                        (5 :| [2, 2, 5, 2, 2, 2])
                  ]
            , _tunings =
                  [
                        Tuning "Flat Forths" "Six-String Guitar"
                              (0 :| [8, 16, 24, 32, 40]) 0
                  ]
            },
            Temperament {
              _temperamentName = "21-TET"
            , _divisions = 21
            , _period = 2 % 1
            , _notationSystems = []
            , _chords = []
            , _scales = 
                  [
                    Scale "Antisinatonic (1L 9s)"
                        (3 :| [2, 2, 2, 2, 2, 2, 2, 2, 2])
                  , Scale "Machinoid (5L 1s)"
                        (4 :| [4, 4, 4, 4, 1])
                  , Scale "p-chro Machinoid (5L 6s)"
                        (3 :| [1, 1, 3, 1, 3, 1, 3, 1, 3, 1])
                  , Scale "Manual (4L 1s)"
                        (5 :| [5, 5, 5, 1])
                  , Scale "Gramitonic (4L 5s)"
                        (4 :| [1, 4, 1, 4, 1, 4, 1, 1])
                  , Scale "Antipentic (3L 2s)"
                        (5 :| [5, 3, 5, 3])
                  , Scale "Oneirotonic (5L 3s)"
                        (3 :| [3, 2, 3, 3, 2, 3, 2])
                  , Scale "LH Diasem Ionian"
                        (3 :| [1, 3, 2, 3, 3, 1, 3, 2])
                  , Scale "LH Diasem Mixo"
                        (3 :| [1, 3, 2, 3, 1, 3, 2, 3])
                  , Scale "LH Diasem Dorian"
                        (1 :| [3, 2, 3, 3, 1, 3, 2, 3])
                  , Scale "LH Diasem Aeolian"
                        (3 :| [2, 3, 1, 3, 2, 3, 3, 1])
                  , Scale "LH Diasem Phrygian"
                        (2 :| [3, 3, 1, 3, 2, 3, 1, 3])
                  , Scale "LH Diasem Lydian"
                        (3 :| [3, 1, 3, 2, 3, 1, 3, 2])
                  , Scale "LH Diasem Darkened Dorian"
                        (3 :| [2, 3, 3, 1, 3, 2, 3, 1])
                  , Scale "LH Diasem Brightened Aeolian"
                        (1 :| [3, 2, 3, 1, 3, 2, 3, 3])
                  , Scale "LH Diasem Locrian"
                        (2 :| [3, 1, 3, 2, 3, 3, 1, 3])
                  , Scale "RH Diasem Ionian"
                        (3 :| [1, 3, 2, 3, 1, 3, 3, 2])
                  , Scale "RH Diasem Mixo"
                        (1 :| [3, 3, 2, 3, 1, 3, 2, 3])
                  , Scale "RH Diasem Dorian"
                        (3 :| [2, 3, 1, 3, 3, 2, 3, 1])
                  , Scale "RH Diasem Aeolian"
                        (3 :| [2, 3, 1, 3, 2, 3, 1, 3])
                  , Scale "RH Diasem Phrygian"
                        (2 :| [3, 1, 3, 3, 2, 3, 1, 3])
                  , Scale "RH Diasem Lydian"
                        (3 :| [1, 3, 3, 2, 3, 1, 3, 2])
                  , Scale "RH Diasem Darkened Mixo"
                        (3 :| [3, 2, 3, 1, 3, 2, 3, 1])
                  , Scale "RH Diasem Brightened Dorian"
                        (1 :| [3, 2, 3, 1, 3, 3, 2, 3])
                  , Scale "RH Diasem Locrian"
                        (2 :| [3, 1, 3, 2, 3, 1, 3, 3])
                  ]
            , _tunings = [
                  Tuning "Standard Tuning" "Six String Guitar"
                        (0 :| [9, 18, 27, 33, 42]) 0
            ]
            },
            Temperament {
              _temperamentName = "22-TET"
            , _divisions = 22
            , _period = 2 % 1
            , _notationSystems =
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
            , _chords =
                  [
                    Chord "Major"
                        (0 :| [7, 6, 9])
                  , Chord "Minor"
                        (0 :| [6, 7, 9])
                  , Chord "SuperMajor"
                        (0 :| [8, 5, 9])
                  , Chord "SubMinor"
                        (0 :| [5, 8, 9])
                  , Chord "Magical"
                        (0 :| [5, 7, 9])
                  , Chord "Tiny"
                        (0 :| [5, 5, 11])
                  , Chord "Giant"
                        (0 :| [8, 7, 6])
                  , Chord "Minor Seventh"
                        (0 :| [6, 7, 6, 3])
                  , Chord "Super Seventh"
                        (0 :| [8, 5, 8, 1])
                  , Chord "Sub Seventh"
                        (0 :| [5, 8, 5, 4])
                  , Chord "Magical Seventh"
                        (0 :| [5, 7, 6, 4])
                  , Chord "Major Super seventh"
                        (0 :| [8, 5, 8, 1])
                  , Chord "Minor Sub Seventh"
                        (0 :| [5, 8, 5, 4])
                  , Chord "Super Minor Seventh"
                        (0 :| [8, 5, 6, 3])
                  , Chord "Sub Major Seventh"
                        (0 :| [5, 8, 6, 3])
                  , Chord "Super Sub Seventh"
                        (0 :| [8, 5, 5, 4])
                  , Chord "Harmonic Seventh"
                        (0 :| [7, 6, 5, 4])
                  , Chord "Tiny seventh"
                        (0 :| [5, 5, 5, 7])
                  , Chord "Giant Sixth"
                        (0 :| [8, 7, 5, 1])
                  , Chord "Harmonic Minor Sixth"
                        (0 :| [6, 7, 5, 4])
                  ]
            , _scales =
                  [
                    Scale "Superpyth[7] (Major)"
                        (4 :| [4, 1, 4, 4, 4, 1])
                  , Scale "Superpyth[7] (Dorian)"
                        (4 :| [1, 4, 4, 4, 1, 4])
                  , Scale "Superpyth[7] (Phrygian)"
                        (1 :| [4, 4, 4, 1, 4, 4])
                  , Scale "Superpyth[7] (Lydian)"
                        (4 :| [4, 4, 1, 4, 4, 1])
                  , Scale "Superpyth[7] (Mixolydian)"
                        (4 :| [4, 1, 4, 4, 1, 4])
                  , Scale "Superpyth[7] (Minor)"
                        (4 :| [1, 4, 4, 1, 4, 4])
                  , Scale "Superpyth[7] (Locrian)"
                        (1 :| [4, 4, 1, 4, 4, 4])
                  , Scale "Maqam Bayati"
                        (3 :| [2, 4, 4, 1, 4, 4])
                  , Scale "Maqam Jiharkah"
                        (4 :| [4, 1, 4, 4, 2, 3])
                  , Scale "Maqam Husayni 'Ushayran"
                        (3 :| [2, 4, 3, 2, 4, 4])
                  , Scale "Maqam Saba"
                        (3 :| [2, 4, 4, 1, 4, 2, 2])
                  , Scale "Maqam Rast"
                        (4 :| [2, 3, 4, 4, 2, 3])
                  , Scale "Syntonic Major"
                        (4 :| [3,2,4,3,4,2])
                  , Scale "Syntonic Dorian"
                        (3 :| [2,4,3,4,2,4])
                  , Scale "Syntonic Phrygian"
                        (2 :| [4,3,4,2,4,3])
                  , Scale "Syntonic Lydian"
                        (4 :| [3,4,2,4,3,2])
                  , Scale "Syntonic Mixolydian"
                        (3 :| [4,2,4,3,2,4])
                  , Scale "Syntonic Minor"
                        (4 :| [2,4,3,2,4,3])
                  , Scale "Syntonic Locrian"
                        (2 :| [4,3,2,4,3,4])
                  , Scale "Superpyth Blues"
                        (5 :| [4, 1, 3, 5, 4])
                  , Scale "Astrology[6]"
                        (4 :| [3, 4, 4, 3, 4])
                  , Scale "Porcupine[7]"
                        (3 :| [3, 3, 4, 3, 3, 3])
                  , Scale "Porcupine[8]"
                        (33333331)
                  , Scale "Orwell[5]"
                        (55255)
                  , Scale "Orwell[9]"
                        (232323232)
                  , Scale "Magic[7]"
                        (1616161)
                  , Scale "Magic[10]"
                        (5115115111)
                  , Scale "Pajara[10]"
                        (2 :| [2, 3, 2, 2, 2, 2, 3, 2, 2])
                  , Scale "Hedgehog[6]"
                        (353353)
                  , Scale "Hedgehog[8]"
                        ( 33323332)
                  , Scale "Astrology[6]"
                        (434434)
                  , Scale "Astrology[10]"
                        (3131331313)
                  , Scale "Doublewide[6]"
                        (551551)
                  , Scale "Doublewide[10]"
                        (4141141411)
                  -- 11-EDO inclusions
                  , Scale ("Machine[6]")
                        (fmap (*2) $ 2 :| [2, 2, 2, 2, 1])
                  , Scale "Orgone[7] (Nerevarine)"
                        (fmap (*2) $ 2 :| [2, 1, 2, 1, 2, 1])
                  , Scale "Orgone[7] (Vivecan)"
                        (fmap (*2) $ 2 :| [1, 2, 2, 1, 2, 1])
                  , Scale "Orgone[7] (Lorkhanic)"
                        (fmap (*2) $ 2 :| [1, 2, 1, 2, 2, 1])
                  , Scale "Orgone[7] (Sothic)"
                        (fmap (*2) $ 2 :| [1, 2, 1, 2, 1, 2])
                  , Scale "Orgone[7] (Kagrenacan)"
                        (fmap (*2) $ 1 :| [2, 2, 1, 2, 1, 2])
                  , Scale "Orgone[7] (Almalexian)"
                        (fmap (*2) $ 1 :| [2, 1, 2, 2, 1, 2])
                  , Scale "Orgone[7] (Dagothic)"
                        (fmap (*2) $ 1 :| [2, 1, 2, 1, 2, 2])
                  , Scale "Joan Pentatonic"
                        (fmap (*2) $ 1 :| [4, 1, 4, 1])
                  , Scale "Joan Heptatonic"
                        (fmap (*2) $ 1 :| [1, 1, 3, 1, 1, 3])
                  , Scale "Joan Nonatonic"
                        (fmap (*2) $ 1 :| [1, 1, 2, 1, 1, 1, 2, 1])
                  ]
            , _tunings =
                  [
                    Tuning "Standard Tuning" "Six-String Guitar"
                        (fmap (+13) $ 0 :| [9, 18, 27, 35, 44]) 0
                  ]
            },
            Temperament {
              _temperamentName = "24-TET"
            , _divisions = 24
            , _period = 2 % 1
            , _notationSystems =
                  [

                  ]
            , _chords =
                  [

                  ]
            , _scales =
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
            , _tunings =
                  [
                    Tuning "Standard Tuning" "Six-String Guitar"
                        (fmap (+14) $ 0 :| [10, 20, 30, 38, 48]) 0
                  , Tuning "Drop D" "Six-String Guitar"
                        (fmap (+12) $ 0 :| [14, 24, 34, 42, 52]) 0
                  ]
            },
            Temperament {
              _temperamentName = "23-TET"
            , _divisions = 23
            , _period = 2 % 1
            , _notationSystems = []
            , _chords = []
            , _scales =
                  [
                    Scale "3L 2s (oneiro-pentatonic)"
                        (5 :| [4, 5, 5, 4])
                  , Scale "4L 1s (bug pentatonic)"
                        (5 :| [5, 5, 5, 3])
                  , Scale "5L 1s (machinoid)"
                        (4 :| [4, 4, 4, 4, 3])
                  , Scale "4L 3s (smitonic)"
                        (5 :| [1, 5, 1, 5, 1, 5])
                  , Scale "1L 6s (antiarcheotonic)"
                        (3 :| [3, 3, 5, 3, 3, 3])
                  , Scale "2L 5s (mavila, anti-diatonic)"
                        (3 :| [3, 4, 3, 3, 3, 4])
                  , Scale "3L 4s (mosh)"
                        (2 :| [5, 2, 5, 2, 5, 2])
                  , Scale "5L 3s (oneirotonic)"
                        (4 :| [1, 4, 4, 1, 4, 4, 1])
                  , Scale "7L 1s (porcupoid)"
                        (3 :| [3, 3, 3, 3, 3, 3, 2])
                  , Scale "7L 2s (mavila superdiatonic)"
                        (3 :| [3, 3, 1, 3, 3, 3, 3, 1])
                  , Scale "5L 4s (bug semiquartal)"
                        (3 :| [2, 3, 2, 3, 2, 3, 2, 3])
                  , Scale "3L 7s (sephiroid)"
                        (3 :| [2, 2, 3, 2, 2, 3, 2, 2, 2])
                  ]
            , _tunings = 
                  [
                        Tuning "Wide Fourths" "Six String Guitar"
                              (0 :| [10, 20, 30, 40, 50]) 0
                  ]
            },
            Temperament {
              _temperamentName = "25-TET"
            , _divisions = 25
            , _period = 2 % 1
            , _notationSystems = []
            , _chords = []
            , _scales = []
            , _tunings = []
            },
            Temperament {
              _temperamentName = "26-TET"
            , _divisions = 26
            , _period = 2 % 1
            , _notationSystems = []
            , _chords = []
            , _scales = 
                  [
                    Scale "Flattone"
                        (4 :| [4, 4, 3, 4, 4, 3])
                  , Scale "Orgone"
                        (5 :| [5, 2, 5, 2, 5, 2])
                  , Scale "Lemba"
                        (5 :| [5, 3, 5, 5, 3])
                  ]
            , _tunings = 
                  [
                    Tuning "All Fourths" "Six String Guitar"
                        (0 :| [11, 22, 33, 44, 55]) 0
                  ]
            },
            Temperament {
              _temperamentName = "27-TET"
            , _divisions = 27
            , _period = 2 % 1
            , _notationSystems = []
            , _chords = []
            , _scales = []
            , _tunings = []
            },
            Temperament {
              _temperamentName = "28-TET"
            , _divisions = 28
            , _period = 2 % 1
            , _notationSystems = []
            , _chords = []
            , _scales = 
                  [
                    Scale "Negri [9]"
                        (3 :| [3, 3, 3, 4, 3, 3, 3, 3])
                  , Scale "Negri [10]"
                        (3 :| [3, 3, 3, 3, 3, 3, 3, 3, 1])
                  , Scale "Diatonic Major [7]"
                        (5 :| [4, 3, 4, 5, 5, 2])
                  , Scale "Diatonic Minor [7]"
                        (5 :| [2, 5, 4, 3, 4, 5])
                  , Scale "Diatonic Naive Major [7]"
                        (4 :| [5, 3, 4, 5, 4, 3])
                  , Scale "Diatonic Naive Minor [7]"
                        (4 :| [3, 5, 4, 3, 4, 5])
                  , Scale "Harmonic Minor [7]"
                        (5 :| [2, 5, 4, 3, 7, 2])
                  , Scale "Harmonic Major [7]"
                        (5 :| [4, 3, 4, 3, 7, 2])
                  , Scale "Diasem (Right-handed)"
                        (4 :| [1, 4, 4, 3, 4, 1, 4, 3])
                  , Scale "Diasem (Left-handed)"
                        (4 :| [4, 1, 4, 3, 4, 1, 4, 3])
                  , Scale "Oneirotonic [5]"
                        (6 :| [5, 6, 5, 6])
                  , Scale "Oneirotonic [8]"
                        (5 :| [5, 1, 5, 5, 1, 5, 1])
                  ]
            , _tunings = 
                  [
                    Tuning "Wide Fourths" "Six String Guitar"
                        (0 :| [12, 24, 36, 48, 69]) 0
                  , Tuning "Narrow Fourths" "Six String Guitar"
                        (0 :| [11, 22, 33, 44, 55]) 0
                  ]
            },
            Temperament {
              _temperamentName = "30-TET"
            , _divisions = 30
            , _period = 2 % 1
            , _notationSystems =
                  [

                  ]
            , _chords = []
            , _scales = 
                  [
                    Scale "Lovecraft[5]"
                        (7 :| [7, 7, 7, 2])
                  , Scale "Lovecraft[9]"
                        (5 :| [2, 5, 2, 5, 2, 5, 2, 2])
                  , Scale "Sensi[5]"
                        (8 :| [3, 8, 3, 8])
                  , Scale "Sensi[8]"
                        (5 :| [3, 3, 5, 3, 3, 5, 3])
                  , Scale "Mavila[5]"
                        (9 :| [4, 9, 4, 4])
                  , Scale "Mavila[7]"
                        (5 :| [4, 4, 5, 4, 4, 4])
                  , Scale "Mavila[9]"
                        (4 :| [4, 4, 4, 1, 4, 4, 4, 1])
                  ]
            , _tunings = 
                  [
                    Tuning "Narrow Fourths" "Six String Guitar"
                        (0 :| [12, 24, 36, 48, 60]) 0
                  ]
            },
            Temperament {
              _temperamentName = "31-TET"
            , _divisions = 31
            , _period = 2 % 1
            , _notationSystems =
                  [

                  ]
            , _chords =
                  [

                  ]
            , _scales =
                  [
                    Scale "Miracle[5]"
                        (3 :| [3, 3, 3, 19])
                  , Scale "Nusecond[5]"
                        (4 :| [4, 4, 4, 15])
                  , Scale "Hemithirds[5]"
                        (5 :| [5, 5, 5, 11])
                  , Scale "Mothra[5]"
                        (6 :| [6, 6, 6, 7])
                  , Scale "Orwell[5]"
                        (7 :| [7, 7, 7, 3])
                  , Scale "Squares[5]"
                        (2 :| [9, 2, 9, 9])
                  , Scale "Semisept[5]"
                        (5 :| [7, 5, 7, 7])
                  , Scale "Meantone[5]"
                        (8 :| [5, 8, 5, 5])
                  , Scale "Casablanca[5]"
                        (11 :| [3, 11, 3, 3])
                  , Scale "Tritonic[5]"
                        (14 :| [1, 14, 1, 1])
                  , Scale "Miracle[6]"
                        (3 :| [3, 3, 3, 3, 16])
                  , Scale "Nusecond[6]"
                        (4 :| [4, 4, 4, 4, 11])
                  , Scale "Hemithirds[6]"
                        (5 :| [5, 5, 5, 5, 6])
                  , Scale "Mothra[6]"
                        (6 :| [6, 6, 6, 6, 1])
                  , Scale "Miracle[7]"
                        (3 :| [3, 3, 3, 3, 3, 13])
                  , Scale "Nusecond[7]"
                        (4 :| [4, 4, 4, 4, 4, 7])
                  , Scale "Hemithirds[7]"
                        (5 :| [5, 5, 5, 5, 5, 1])
                  , Scale "Myna[7]"
                        (1 :| [7, 1, 7, 1, 7, 7])
                  , Scale "Mohajira[7]"
                        (5 :| [4, 5, 4, 5, 4, 4])
                  , Scale "Würschmidt[7]"
                        (9 :| [1, 9, 1, 9, 1, 1])
                  , Scale "Meantone[7]"
                        (3 :| [5, 5, 3, 5, 5, 5])
                  , Scale "Casablanca[7]"
                        (8 :| [3, 3, 8, 3, 3, 3])
                  , Scale "Tritonic[7]"
                        (13 :| [1, 1, 13, 1, 1, 1])
                  , Scale "Miracle[8]"
                        (3 :| [3, 3, 3, 3, 3, 3, 10])
                  , Scale "Nusecond[8]"
                        (4 :| [4, 4, 4, 4, 4, 4, 3])
                  , Scale "Squares[8]"
                        (2 :| [2, 7, 2, 2, 7, 2, 7])
                  , Scale "Semisept[8]"
                        (5 :| [5, 2, 5, 5, 2, 5, 2])
                  , Scale "Miracle[9]"
                        (3 :| [3, 3, 3, 3, 3, 3, 3, 7])
                  , Scale "Orwell[9]"
                        (4 :| [3, 4, 3, 4, 3, 4, 3, 3])
                  , Scale "Casablanca[9]"
                        (5 :| [3, 3, 3, 5, 3, 3, 3, 3])
                  ]
            , _tunings =
                  [
                    Tuning "Standard Tuning" "Six String Guitar"
                        (0 :| [13, 26, 39, 49, 62]) 0
                  ]
            },
            Temperament {
              _temperamentName = "34-TET"
            , _divisions = 34
            , _period = 2 % 1
            , _notationSystems = []
            , _chords = []
            , _scales = []
            , _tunings = []
            },
            Temperament {
              _temperamentName = "36-TET"
            , _divisions = 36
            , _period = 2 % 1
            , _notationSystems = []
            , _chords = []
            , _scales = []
            , _tunings = []
            },
            Temperament {
              _temperamentName = "41-TET"
            , _divisions = 41
            , _period = 2 % 1
            , _notationSystems =
                  [

                  ]
            , _chords =
                  [

                  ]
            , _scales =
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
            , _tunings =
                  [
                    Tuning "Standard Tuning" "Kite Guitar"
                        (0 :| [13, 26, 39, 52, 65]) 1
                  ]
            },
            Temperament {
              _temperamentName = "Bohlen Pierce"
            , _divisions = 13
            , _period = 3 % 1
            , _notationSystems =
                  [
                    NotationSystem "Standard" [
                      "A","A#","B","C","C#","D","E","F","F#","G","H","H#","J"
                    ]
                  ]
            , _chords =
                  [

                  ]
            , _scales =
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
            , _tunings =
                  [
                    Tuning "Bohlen's Tuning" "Six String Guitar"
                        (0 :| [3,6,9,13,16]) 0
                  ]
            }
        ]
    , _preferences = defaultPreferences
}