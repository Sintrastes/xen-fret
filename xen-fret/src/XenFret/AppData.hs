
module XenFret.AppData where

import Data.Aeson.TH
import XenFret.Data
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
            Temperament "11-TET" 11 (2 % 1) 
                  (Just [
                      "Q/P#","Q#/Rb","R","R#/Sb","S",
                      "S#/Tb","T","T#/Ub","U","U#/Pb","P/Qb"
                    ]),
            Temperament "12-TET" 12 (2 % 1)
                  (Just [
                      "A","A#/Bb","B","C","C#/Db","D",
                      "D#/Eb","E","F","F#/Gb","G","G#/Ab"
                    ]),
            Temperament "13-TET" 13 (2 % 1) 
                  (Just [
                       "J","J#/Kb","K","L","L#/Mb","M","M#/Nb",
                       "N","O","O#/Pb","P","Q","Q#/Jb"
                    ]),
            Temperament "14-TET" 14 (2 % 1) 
                  (Just [
                     "A","^A","B","^B","C","^C","D","^D","E","^E",
                     "F","^F","G","^G"
                  ]),
            Temperament "15-TET" 15 (2 % 1) 
                  (Just [
                    "α","β\\","β","χ\\","χ","δ\\","δ",
                    "ε\\","ε","φ\\","φ","γ\\","γ","η\\",
                    "η"
                  ]),
            Temperament "16-TET" 16 (2 % 1) 
                  (Just [
                      "A","B#","B","Bb","C#","C","D#","D",
                      "E#","E","Eb","F#","F","G#","G","A#"
                  ]),
            Temperament "17-TET" 17 (2 % 1) 
                  (Just [
                    "A","Bb","A#","B","C","Db","C#","D",
                    "Eb","D#","E","F","Gb","F#","G","Ab",
                    "G#"
                  ]),
            Temperament "18-TET" 18 (2 % 1) 
                  (Just [
                        "A","Bb","A#","B","C","Db","C#","D",
                        "Eb","D#","E","F","Gb","F#","G",
                        "Hb","G#","H"
                  ]),
            Temperament "19-TET" 19 (2 % 1) 
                  (Just [
                      "A","A#","Bb","B","B#","C",
                      "C#","Db","D","D#","Eb","E",
                      "E#","F","F#","Gb","G","G#","Ab"                  
                  ]),
            Temperament "22-TET" 22 (2 % 1) 
                  (Just [
                    "A","A#","Bb","B","B#",
                    "Cb","C","C#","Db","D",
                    "D#","Eb","E","E#","Fb",
                    "F","F#","Gb","G","G#",
                    "Gx","Ab"
                  ]),
            Temperament "24-TET" 24 (2 % 1) 
                  Nothing,
            Temperament "Bohlen Pierce" 13 (3 % 1) 
                  (Just [
                    "A","A#","B","C","C#","D","E","F","F#","G","H","H#","J"
                  ])
        ]
    , tunings = toMap $ fromList 
        [
            ("11-TET", Tuning "Wide Fourths Tuning" "Six-String Guitar" 
                (0 :| [5, 10, 15, 20, 25])),
            ("11-TET", Tuning "Major Thirds Tuning" "Six-String Guitar" 
                (0 :| [4, 8, 12, 16, 20])),
            ("11-TET", Tuning "Wide Fourths Tuning (Bass)" "Four-String Bass Guitar" 
                (0 :| [5, 10, 15])),
            ("11-TET", Tuning "Major Thirds Tuning (Bass)" "Four-String Bass Guitar" 
                (0 :| [4, 8, 120])),
            ("12-TET", Tuning "Standard Tuning" "Mandolin"
                (fmap (+10) $ 0 :| [7, 14, 21])),
            ("12-TET", Tuning "Standard Tuning" "Six-String Guitar" 
                (fmap (+7) $ 0 :| [5, 10, 15, 19, 24])),
            ("12-TET", Tuning "Standard Tuning (Bass)" "Four-String Bass Guitar" 
                (fmap (+7) $ 0 :| [5, 10, 15])),
            ("12-TET", Tuning "Standard Tuning (7 String)" "Seven-String Guitar" 
                (fmap (+2) $ 0 :| [5, 10, 15, 20, 14, 29])),
            ("12-TET", Tuning "Drop D" "Six-String Guitar" 
                (fmap (+5) $ 0 :| [7, 12, 17, 21, 26])),
            ("12-TET", Tuning "DADGAD" "Six-String Guitar" 
                (fmap (+5) $ 0 :| [7, 12, 17, 19, 24])),
            ("12-TET", Tuning "All Fourths" "Six-String Guitar" 
                (fmap (+7) $ 0 :| [5, 10, 15, 20, 25])),
            ("12-TET", Tuning "All Fifths" "Six-String Guitar" 
                (0 :| [7, 14, 21, 28, 35])),
            ("13-TET", Tuning "Oneirotonic Tuning" "Six-String Guitar"
                (3 :| [8, 14, 19, 24, 29])),
            ("14-TET", Tuning "Wide Fourths Tuning" "Six-String Guitar"
                (0 :| [5, 5, 5, 5, 5])),
            ("15-TET", Tuning "All Fourths Tuning" "Six-String Guitar"
                (0 :| [5, 10, 15, 20, 25])),
            ("16-TET", Tuning "Wide Fourths Tuning" "Six-String Guitar"
                (fmap (+9) $ 0 :| [7, 14, 21, 28, 35])),
            ("16-TET", Tuning "Diminished Fourths Tuning" "Six-String Guitar"
                (fmap (+9) $ 0 :| [6, 12, 18, 24, 30])),
            ("16-TET", Tuning "Wide Fourths Tuning (7 String)" "Seven-String Guitar"
                (fmap (+9) $ 0 :| [7, 14, 21, 28, 35, 40])),
            ("16-TET", Tuning "Diminished Fourths Tuning (7 String)" "Seven-String Guitar"
                (fmap (+2) $ 0 :| [6, 12, 18, 24, 30, 36])),
            ("17-TET", Tuning "Standard Tuning" "Six-String Guitar"
                (fmap (+10) $ 0 :| [7, 14, 21, 27, 34])),
            ("17-TET", Tuning "All Fourths" "Six-String Guitar"
                (fmap (+10) $ 0 :| [7, 14, 21, 28, 35])), 
            ("18-TET", Tuning "Wide Fourths" "Six-String Guitar"
                (0 :| [8, 16, 24, 32, 40])),
            ("19-TET", Tuning "Standard Tuning" "Six-String Guitar"
                (fmap (+11) $ 0 :| [8, 16, 24, 30, 38])),
            ("22-TET", Tuning "Standard Tuning (22-TET)" "Six-String Guitar"
                (fmap (+13) $ 0 :| [9, 18, 27, 35, 44])),
            ("24-TET", Tuning "Standard Tuning" "Six-String Guitar" 
                (fmap (+14) $ 0 :| [10, 20, 30, 38, 48])),
            ("24-TET", Tuning "Drop D" "Six-String Guitar" 
                (fmap (+12) $ 0 :| [14, 24, 34, 42, 52])),
            ("Bohlen Pierce", Tuning "Bohlen's Tuning" "Six String Guitar"
                (0 :| [3,6,9,13,16])) -- A1 C E G A C
        ]
    , scales = toMap $ fromList 
        [
            ("11-TET", Scale "Orgone[7]"
                (1 :| [2, 1, 2, 1, 2, 2]))
          , ("11-TET", Scale "Machine[5]"
                (2 :| [2, 2, 2, 3]))
          , ("11-TET", Scale "Machine[6]"
                (2 :| [2, 2, 2, 2, 1]))
          , ("11-TET", Scale "Joan heptatonic"
                (1 :| [1, 1, 3, 1, 1, 3]))
          , ("11-TET", Scale "Joan pentatonic"
                (1 :| [4, 1, 4, 1]))
          , ("12-TET", Scale "Ionian (Major)" 
                (2 :| [2, 1, 2, 2, 2, 1]))
          , ("12-TET", Scale "Mixolydian" 
                (2 :| [2, 1, 2, 2, 1, 2]))
          , ("12-TET", Scale "Minor" 
                (2 :| [1, 2, 2, 1, 2, 2]))
          , ("12-TET", Scale "Dorian" 
                (2 :| [1, 2, 2, 2, 1, 2]))
          , ("13-TET", Scale "Archeotonic (Ryonian Mode)"
                (2 :| [2, 2, 2, 2, 2, 1]))
          , ("13-TET", Scale "Oneirotonic (Dylathian Mode)"
                (2 :| [2, 1, 2, 2, 1, 2, 1]))
          , ("14-TET", Scale "Titanium[9]"
                (2 :| [1, 2, 1, 2, 1, 2, 1, 2]))
          , ("15-TET", Scale "Augmented[6]"
                (4 :| [1, 4, 1, 4, 1]))
          , ("15-TET", Scale "Triforce[6]"
                (3 :| [2, 3, 2, 3, 2]))
          , ("15-TET", Scale "Porcupine[7]"
                (3 :| [2, 2, 2, 2, 2, 2]))
          , ("15-TET", Scale "Orgone[7]"
                (1 :| [3, 1, 3, 1, 3, 3]))
          , ("15-TET", Scale "Porcupine[8]"
                (2 :| [1, 2, 2, 2, 2, 2, 2]))
          , ("15-TET", Scale "Augmented[9]"
                (3 :| [1, 1, 3, 1, 1, 3, 1, 1]))
          , ("15-TET", Scale "Triforce[9]"
                (2 :| [1, 2, 2, 1, 2, 2, 1, 2]))
          , ("15-TET", Scale "Blackwood[10]"
                (2 :| [1, 2, 1, 2, 1, 2, 1, 2, 1]))
          , ("15-TET", Scale "Marvel double harmonic major"
                  (1 :| [4,1,3,1,4,1]))
          , ("16-TET", Scale "mavila[7]"
                (2 :| [2, 2, 3, 2, 2, 3]))
          , ("16-TET", Scale "Lemba"
                (3 :| [3, 2, 3, 3, 2]))
          , ("16-TET", Scale "Magic"
                (1 :| [4, 1, 4, 1, 4, 1]))
          , ("17-TET", Scale "Major"
                 (3 :| [3, 3, 1, 3, 3, 1]))
          , ("17-TET", Scale "Maqamic[7]"
                 (3 :| [2, 3, 2, 3, 2, 2]))
          , ("17-TET", Scale "Squares[8]"
                  (1 :| [1, 4, 1, 4, 1, 4]))
          , ("17-TET", Scale "Hydra"
                  (3 :| [3, 1, 1, 2, 3, 2, 1, 1]))
          , ("17-TET", Scale "Springfieldian"
                  (3 :| [3, 2, 2, 3, 3, 1]))
          , ("17-TET", Scale "Northhaverbrookian"
                  (2 :| [3, 3, 1, 3, 3, 2]))
          , ("17-TET", Scale "Shelbyvillean"
                  (3 :| [3, 1, 3, 3, 2, 2]))
          , ("18-TET", Scale "Bicycle"
                  (4 :| [4, 1, 4, 4, 1]))
          , ("18-TET", Scale "Mish Heptatonic"
                  (3 :| [2, 3, 2, 3, 3, 2]))
          , ("19-TET", Scale "Ionian (Major)"
                (3 :| [3, 2, 3, 3, 3, 2]))
          , ("19-TET", Scale "Sensi[8]"
                (2 :| [3, 2, 2, 3, 2, 2, 3]))
          , ("19-TET", Scale "Negri[9]"
                (2 :| [2, 2, 2, 3, 2, 2, 2, 2]))
          , ("19-TET", Scale "Kleismic[7]"
                (1 :| [4, 1, 4, 1, 4, 4]))
          , ("22-TET", Scale "Superpyth[7] (Major)"
                (4 :| [4, 1, 4, 4, 4, 1]))
          , ("22-TET", Scale "Astrology[6]"
                (4 :| [3, 4, 4, 3, 4]))
          , ("22-TET", Scale "Porcupine[7]"
                (3 :| [3, 3, 4, 3, 3, 3]))
          , ("22-TET", Scale "Pajara[10]"
                (2 :| [2, 3, 2, 2, 2, 2, 3, 2, 2]))
          , ("24-TET", Scale "Ionian (Major)"
                (4 :| [4, 2, 4, 4, 4, 2]))
          , ("24-TET", Scale "Anchihoye: Ethiopia"
                (2 :| [8, 3, 6, 5]))
          , ("24-TET", Scale "Enharmonic Phrygian"
                (8 :| [1, 1, 8, 4, 1, 1]))
          , ("24-TET", Scale "Maqam Rast"
                (4 :| [3, 3, 4, 4, 3, 3]))
          , ("24-TET", Scale "Mohajira[7]"
                (3 :| [4, 3, 4, 3, 4, 3]))
          , ("Bohlen Pierce", Scale "Lambda"
                (2 :| [1, 1, 2, 1, 2, 1, 2, 1]))
          , ("Bohlen Pierce", Scale "Moll 1"
                  (1 :| [2,1,2,1,2,1,2,1]))
          , ("Bohlen Pierce", Scale "Harmonic"
                  (1 :| [2,1,2,1,2,1,1,2]))
          , ("Bohlen Pierce", Scale "Dur I"
                  (1 :| [2,1,2,1,1,2,1,2]))
          , ("Bohlen Pierce", Scale "Moll 2"
                  (2 :| [1,2,1,1,2,1,2,1]))
          , ("Bohlen Pierce", Scale "Dur II"
                  (2 :| [1,1,2,1,2,1,1,2]))
          , ("Bohlen Pierce", Scale "Gamma"
                  (1 :| [2,1,2,1,1,2,2,1]))
          , ("Bohlen Pierce", Scale "Walker A"
                  (1 :| [1,2,1,2,1,2,1,2]))
          , ("Bohlen Pierce", Scale "Walker B"
                  (1 :| [2,1,1,2,1,2,1,2]))
          , ("Bohlen Pierce", Scale "Walker I"
                  (2 :| [1,2,1,2,1,2,1,1]))
          , ("Bohlen Pierce", Scale "Walker II"
                  (2 :| [1,2,1,2,1,1,2,1]))
        ]
    , preferences = defaultPreferences
}

defaultPreferences = PreferenceData {
    useDarkMode = False
}