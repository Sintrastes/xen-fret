
module XenFret.App.Pages.Tunings where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.List.NonEmpty (NonEmpty(..))
import Data.Validation
import qualified Data.List.NonEmpty as NE
import XenFret.App.Widgets.Fretboard
import XenFret.Data
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

tuningPage :: _ => FilePath -> m ()
tuningPage appDir = mdo
    appData <- loadAppData (appDir <> "/app_data.json")
    initialTunings <- getTunings (appDir <> "/app_data.json")

    newTuningEvent <- button "New Tuning"

    addedTuning <- switch . current <$> prerender (pure never) (performEvent $ newTuningSubmitted <&> \case
        Nothing -> getTunings appDir
        Just (temperament, tuning) -> do
            toast "Added new tuning"
            currentTunings <- getTunings appDir
            pure $ addTuning temperament tuning currentTunings)

    let updatedTunings = leftmost [addedTuning, deletedEvent, editedTuning]

    dynTunings <- holdDyn initialTunings
        updatedTunings

    let isNewName initialName = dynTunings <&> \tunings temperament instrument name ->
          let names = fmap tuningName (fromMaybe [] $ Map.lookup (temperamentName temperament) tunings) in
            if name `elem` names && Just name /= initialName
                then Failure $ ("There is already a " <> instrument <>
                        " tuning with this name for " <> temperamentName temperament <> ".") :| []
                else Success name

    newTuningSubmitted <- validatedModal newTuningEvent $ \_ -> do
        tuningForm appData (head $ _temperaments appData) (isNewName Nothing) def

    let dynAppData = dynTunings <&> \t ->
           appData { _temperaments = setTunings t (_temperaments appData) }

    persistAppData dynAppData
       (appDir <> "/app_data.json")

    (editEvents, deleteEvents) <- fanEither <$> (switchHold never =<< dyn (dynTunings <&> \currentTunings ->
        elClass "ul" "collection" $ do
            deleteEvents <- forM (Map.toList currentTunings) $ \(temperamentName, tunings) -> do
                el "h3" $ text temperamentName
                forM tunings $ \tuning -> do
                    elClass "li" "collection-item" $ do
                        deleteEvent <- domEvent Click . fst <$> elClass' "i" "material-icons" (
                            text "clear")
                        editEvent <- domEvent Click . fst <$> elClass' "i" "material-icons" 
                            (text "edit")
                        el "span" $ text $
                            "(" <> instrument tuning <> ") "
                        el "span" $ text $
                            T.pack $ show tuning
                        pure $ leftmost
                            [
                              deleteEvent $> Right (temperamentName, tuning)
                            , editEvent $> Left (temperamentName, tuning)
                            ]
            pure $ leftmost (join deleteEvents)))

    let deletedEvent = pushAlways (\(temperamentName, deletedTuning) -> do
            currentTunings <- getTunings appDir

            let tunings = currentTunings Map.! temperamentName
            let updatedTunings = Map.insert temperamentName
                    (filter (/= deletedTuning) tunings) currentTunings

            pure updatedTunings) deleteEvents

    completeEditDialog <- validatedModal editEvents $ \(temperament, tuning) -> do
        res <- tuningForm appData
            (fromJust $ find (\x -> temperamentName x == temperament) $ _temperaments appData)
            (isNewName $ Just $ tuningName tuning) tuning
        pure $ fmap (\(x,y) -> (x, tuning, y)) <$> res

    let editSubmitted = mapMaybe id completeEditDialog

    let editedTuning = pushAlways (\(temperament, origTuning, newTuning) -> do
            currentTunings <- getTunings appDir
            let tunings = currentTunings Map.! temperamentName temperament
            let indexOfScale = fromJust $ tunings & elemIndex origTuning
            let updatedTunings = Seq.fromList tunings
                    & Seq.update indexOfScale newTuning
                    & toList

            let updatedMap = Map.insert 
                    (temperamentName temperament)
                    updatedTunings currentTunings

            pure updatedMap) editSubmitted

    blank

-- | Helper function to add a tuning to the given temperament.
addTuning :: Temperament -> Tuning -> Map T.Text [Tuning] -> Map T.Text [Tuning]
addTuning Temperament{..} tuning map = case Map.lookup temperamentName map of
  Nothing -> Map.insert temperamentName [tuning] map
  Just tuns -> Map.insert temperamentName (tuns ++ [tuning]) map

tuningForm :: MonadWidget t m => AppData
    -> Temperament
    -> Dynamic t (Temperament -> T.Text -> T.Text -> Validation (NonEmpty ErrorMessage) T.Text)
    -> Tuning
    -> m (
        Dynamic t (Validation (NonEmpty ErrorMessage) (Temperament, Tuning))
    )
tuningForm appData initialTemperament isNewName initialValue = do
    modalHeader "Add New Tuning"

    temperamentForm <- (validateNonNull "Temperament must be selected" <$>) <$>
            selectTemperament appData initialTemperament

    let temperamentUpdated = fmapMaybe validationToMaybe
            (updated temperamentForm)

    currentTemperament <- holdDyn initialTemperament =<<
        delay 0.1 temperamentUpdated

    let currentInstrument = pure ""

    let tuningValidation = combineDynValidations
          [ pure $ nonEmptyText "Tuning name must not be empty"
          , isNewName <*> currentTemperament <*> currentInstrument]

    nameDyn <- (labeledEntryA "Name" (validatedTextEntryDyn tuningValidation id))
        (tuningName initialValue)

    instrumentDyn <- (labeledEntryA "Instrument" (
          nonEmptyTextEntry
              "Instrument name must not be empty"))
          (instrument initialValue)

    intervalsDyn <- labeledEntryA "Intervals"
        intervalListEntry
        (stringTunings initialValue)

    skipFrettingDyn <- labeledEntry "Skip Fretting"
        intEntry 0

    let tuningFrom = mkTuningForm
            nameDyn instrumentDyn
            intervalsDyn 
            (Success <$> skipFrettingDyn)

    pure $ pairForms temperamentForm tuningFrom

mkTuningForm :: (Semigroup e, Reflex t) =>
     Dynamic t (Validation e T.Text)
  -> Dynamic t (Validation e T.Text)
  -> Dynamic t (Validation e (NonEmpty Int))
  -> Dynamic t (Validation e Int)
  -> Dynamic t (Validation e Tuning)
mkTuningForm name instrument strings skipFretting = do
    name' <- name
    instrument' <- instrument
    strings' <- strings
    skipFretting' <- skipFretting

    return $ Tuning <$>
        name' <*>
        instrument' <*>
        strings' <*>
        skipFretting'