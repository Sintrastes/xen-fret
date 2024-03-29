
module XenFret.App.Pages.Chords where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.List.NonEmpty (NonEmpty(..))
import Data.Validation
import qualified Data.List.NonEmpty as NE
import XenFret.App.Widgets.Fretboard
import XenFret.Data
import XenFret.AppData
import XenFret.Util
import XenFret.App.Util
import XenFret.AppData
import Reflex.Dom.Core hiding(Home, button, checkbox)
import Reflex.Dom.Extras
import Reflex.Dom.Forms
import qualified Data.Sequence as Seq
import Data.List
import Data.Maybe hiding (mapMaybe)
import Data.Functor
import Control.Monad
import Data.Foldable
import XenFret.App.Widgets.CRUD
import Control.Lens
import Debug.Trace
import XenFret.App.Util

-- Helper "Iso" (really not a strict isomorphism)
--  to help reduce "at"s that return a list.
reduceMaybe :: Iso' (Maybe [a]) [a]
reduceMaybe = iso (fromMaybe []) Just

chordPage :: _ => FilePath -> m ()
chordPage appDir = crudPage appDir "chord" chordForm
    (\x -> lens
      (maybe [] _chords . find (\ t -> _temperamentName t == x) . _temperaments)
      (\appData newChords ->
          case find (\t -> _temperamentName t == x) (_temperaments appData) of
              Just y  -> appData { _temperaments = updateTemperament (_temperaments appData) (y { _chords = newChords }) }
              Nothing -> appData
      )
    )

updateTemperament :: [Temperament] -> Temperament -> [Temperament]
updateTemperament ts t = (\x -> if _temperamentName x == _temperamentName t then t else x) <$> ts

chordForm :: MonadWidget t m =>
    AppData
 -> Temperament
 -> (T.Text -> Dynamic t [T.Text])
 -> Chord
 -> m (Dynamic t (Validation (NonEmpty ErrorMessage) (Temperament, Chord)))
chordForm appData initialTemperament currentChords initialValue = mdo
    modalHeader "Add New Chord"

    currentTemperament <- holdDyn initialTemperament =<<
        delay 0.1 temperamentUpdated

    let isValidName = \temperament -> do
          chords <- currentChords (_temperamentName temperament)
          pure $ trace (show chords) $ \x -> if x `elem` chords
            then Failure $ "There is already a chord with this name." :| []
            else Success x

    let chordValidation = combineDynValidations
          [ pure $ nonEmptyText "Chord name must not be entry"
          , isValidName =<< currentTemperament
          ]

    temperamentForm <- (validateNonNull "Temperament must be selected" <$>) <$>
            selectTemperament appData initialTemperament

    let temperamentUpdated = fmapMaybe validationToMaybe
            (updated temperamentForm)

    let periodDyn = currentTemperament <&> _divisions

    chordForm <- initFormA (Chord <$>
          formA (chordName =. labeledEntryA "Name"
            (validatedTextEntryDyn chordValidation id)) <*>
          formA (chordIntervals =. labeledEntryA "Intervals"
            (scaleListEntry periodDyn))) initialValue

    pure $ pairForms temperamentForm chordForm