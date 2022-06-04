
module XenFret.Data where

import qualified Data.Text as T
import Data.Default
import Data.Aeson.TH
import Data.List.NonEmpty
import Data.Ratio

data Temperament = Temperament {
    temperamentName :: T.Text, 
    divisions :: Int,
    period :: Rational
}
    deriving(Eq)

instance Default Temperament where
    def = Temperament {
        temperamentName = "",
        divisions = 12,
        period = 2 % 1
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

$(deriveJSON defaultOptions ''Tuning)

data Scale = Scale {
    scaleName  :: T.Text,
    scaleIntervals :: NonEmpty Int
}
    deriving(Eq)

instance Show Scale where
    show = T.unpack . scaleName

$(deriveJSON defaultOptions ''Scale)