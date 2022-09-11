{-# OPTIONS_GHC -Wno-name-shadowing #-}

module XenFret.Data where

import qualified Data.Text as T
import Data.Default
import Data.Aeson.TH
import Data.List.NonEmpty hiding((!!), cycle)
import Data.Ratio
import Control.Lens.TH

class Named a where
    name :: a -> T.Text

data NotationSystem = NotationSystem {
    notationName :: T.Text,
    noteNames :: [T.Text]
}
    deriving(Eq)

instance Show NotationSystem where
    show = T.unpack . notationName

instance Default NotationSystem where
    def = NotationSystem {
        notationName = "",
        noteNames = []
    }

$(deriveJSON defaultOptions ''NotationSystem)

data Tuning = Tuning {
    tuningName :: T.Text,
    instrument :: T.Text,
    stringTunings :: NonEmpty Int,
    skipFrets :: Int
}
    deriving(Eq)

instance Show Tuning where
    show = T.unpack . tuningName

instance Default Tuning where
    def = Tuning {
        tuningName = "",
        instrument = "",
        stringTunings = 0 :| [],
        skipFrets = 0
    }

$(deriveJSON defaultOptions ''Tuning)

data Scale = Scale {
    scaleName  :: T.Text,
    scaleIntervals :: NonEmpty Int
}
    deriving(Eq)

instance Show Scale where
    show = T.unpack . scaleName

instance Default Scale where
    def = Scale "" (0 :| [])

$(deriveJSON defaultOptions ''Scale)

data Chord = Chord {
    chordName  :: T.Text,
    chordIntervals :: NonEmpty Int
}
    deriving(Eq)

instance Named Chord where
    name = chordName

instance Show Chord where
    show = T.unpack . chordName

toScale (Chord name intervals) = Scale name intervals

instance Default Chord where
    def = Chord "" (0 :| [])

$(deriveJSON defaultOptions ''Chord)

data Temperament = Temperament {
    _temperamentName :: T.Text, 
    _divisions :: Int,
    _period :: Rational,
    _notationSystems :: [NotationSystem],
    _tunings :: [Tuning],
    _scales  :: [Scale],
    _chords :: [Chord]
}
    deriving(Eq)

$(makeLenses ''Temperament)

type NoteNames = Maybe [String]

displayNote :: (?noteNames :: NoteNames) => Int -> String
displayNote note = 
    let noteNames = maybe (fmap show [(0 :: Int)..]) cycle ?noteNames 
     in noteNames !! note

instance Default Temperament where
    def = Temperament {
        _temperamentName = "",
        _divisions = 12,
        _period = 2 % 1,
        _notationSystems = []
    }

instance Show Temperament where
    show = T.unpack . _temperamentName

$(deriveJSON defaultOptions ''Temperament)