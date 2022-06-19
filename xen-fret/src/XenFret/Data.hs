{-# OPTIONS_GHC -Wno-name-shadowing #-}

module XenFret.Data where

import qualified Data.Text as T
import Data.Default
import Data.Aeson.TH
import Data.List.NonEmpty hiding((!!), cycle)
import Data.Ratio

data Temperament = Temperament {
    temperamentName :: T.Text, 
    divisions :: Int,
    period :: Rational,
    noteNames :: Maybe [T.Text]
}
    deriving(Eq)

type NoteNames = Maybe [String]

displayNote :: (?noteNames :: NoteNames) => Int -> String
displayNote note = 
    let noteNames = maybe (fmap show [(0 :: Int)..]) cycle ?noteNames 
     in noteNames !! note

instance Default Temperament where
    def = Temperament {
        temperamentName = "",
        divisions = 12,
        period = 2 % 1,
        noteNames = Nothing
    }

instance Show Temperament where
    show = T.unpack . temperamentName

$(deriveJSON defaultOptions ''Temperament)

data Tuning = Tuning {
    tuningName :: T.Text,
    instrument :: T.Text,
    stringTunings :: NonEmpty Int
}
    deriving(Eq)

instance Show Tuning where
    show = T.unpack . tuningName

instance Default Tuning where
    def = Tuning {
        tuningName = "",
        instrument = "",
        stringTunings = 0 :| []
    }

$(deriveJSON defaultOptions ''Tuning)

data Scale = Scale {
    scaleName  :: T.Text,
    scaleIntervals :: NonEmpty Int
}
    deriving(Eq)

instance Show Scale where
    show = T.unpack . scaleName

$(deriveJSON defaultOptions ''Scale)