
module XenFret.App.Pages.Temperaments where

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
import XenFret.App.Widgets.Fretboard

temperamentPage :: _ => FilePath -> m ()
temperamentPage appDir = mdo
    appData <- loadAppData (appDir <> "/app_data.json")
    let initialTemperaments = _temperaments appData
    let initialScales = _scales appData
    let initialTunings = _tunings appData

    newTemperamentEvent <- button "New Temperament"

    newTemperament <- switch . current <$> prerender (pure never) (performEvent $ newTemperamentSubmitted <&> \case
        Nothing -> pure initialTemperaments
        Just temperament -> do
            toast "Added new temperament"
            currentTemperament <- getTemperaments appDir
            pure (currentTemperament ++ [temperament]))

    let updatedTemperaments = leftmost [newTemperament, deleteEvent]

    dynTemperaments <- holdDyn initialTemperaments
        (leftmost [updatedTemperaments, editedTemperaments])

    let isNewName initialName = dynTemperaments <&> \ temperaments name ->
            let names = fmap temperamentName temperaments in
                if name `elem` names && initialName /= Just name
                    then Failure $ "There is already a temperament with this name." :| []
                    else Success name

    newTemperamentSubmitted <- validatedModal newTemperamentEvent $ \_ ->
        temperamentForm (isNewName Nothing) def

    (editEvent, deleteClickedEvent) <- fanEither <$> (switchHold never =<< dyn (dynTemperaments <&> \currentTemperaments ->
        elClass "ul" "collection" $ do
            itemEvents <- forM currentTemperaments (\temperament -> do
                elClass "li" "collection-item" $ do
                    deleteEvent <- domEvent Click . fst <$> elClass' "i" "material-icons" (
                        text "clear")
                    editEvent <- domEvent Click . fst <$> elClass' "i" "material-icons" 
                        (text "edit")
                    el "span" $ text $
                        T.pack $ show temperament
                    pure $ leftmost 
                        [
                            deleteEvent $> Right temperament
                          , editEvent   $> Left temperament
                        ])
            pure $ leftmost itemEvents))

    -- Determine events for how various data sources need to be updated
    -- when deleting a temperament.
    let updatedEvents = pushAlways (\toDelete -> do
            currentTemperaments <- getTemperaments appDir
            currentTunings <- getTunings appDir
            currentScales <- getScales appDir

            let updatedTemperaments = filter (/= toDelete) currentTemperaments
            let updatedTunings = Map.delete (temperamentName toDelete) currentTunings
            let updatedScales = Map.delete (temperamentName toDelete) currentScales

            pure (updatedTemperaments, updatedTunings, updatedScales))
                deleteClickedEvent

    completeEditDialog <- validatedModal editEvent $ \temperament -> do
        res <- temperamentForm (isNewName $ Just $ temperamentName temperament) temperament
        pure $ fmap (\x -> (x, temperament)) <$> res

    let editSubmitted = mapMaybe id completeEditDialog

    let editedEvents = pushAlways (\(newTemperament, oldTemperament) -> do
            currentTemperaments <- getTemperaments appDir
            currentTunings <- getTunings appDir
            currentScales <- getScales appDir

            let indexOfTemperament = fromJust $ elemIndex oldTemperament 
                    currentTemperaments

            let updatedTemperaments = currentTemperaments
                    & Seq.fromList
                    & Seq.update indexOfTemperament newTemperament
                    & toList

            let oldName = temperamentName oldTemperament
            let newName = temperamentName newTemperament

            let hasNewName = newName /= oldName

            let updatedTunings = if hasNewName 
                then currentTunings
                    & Map.delete oldName
                    & Map.insert newName (currentTunings Map.! oldName)
                else currentTunings

            let updatedScales = if hasNewName 
                then currentScales
                    & Map.delete oldName
                    & Map.insert newName (currentScales Map.! oldName)
                else currentScales
        
            pure (updatedTemperaments, updatedTunings, updatedScales)) editSubmitted

    let editedTemperaments = (\(x,_,_) -> x) <$> editedEvents
    let editedTunings      = (\(_,y,_) -> y) <$> editedEvents
    let editedScales       = (\(_,_,z) -> z) <$> editedEvents

    let deleteEvent    = (\(x,_,_) -> x) <$> updatedEvents
    let updatedTunings = (\(_,y,_) -> y) <$> updatedEvents
    let updatedScales  = (\(_,_,z) -> z) <$> updatedEvents

    dynTunings <- holdDyn initialTunings
        (leftmost [updatedTunings, editedTunings])

    dynScales <- holdDyn initialScales
        (leftmost [updatedScales, editedScales])

    let dynData = (,,) <$>
            dynTemperaments <*>
            dynTunings <*>
            dynScales

    let dynAppData = dynData <&> \(temperaments, tunings, scales) -> appData {
        _temperaments = temperaments,
        _tunings = tunings,
        _scales = scales
    }

    persistAppData dynAppData
        (appDir <> "/app_data.json")

    blank

temperamentForm :: _ =>
     Dynamic t (T.Text -> Validation (NonEmpty ErrorMessage) T.Text)
  -> Temperament
  -> m (Dynamic t (Validation (NonEmpty ErrorMessage) Temperament))
temperamentForm isNewName initialValue = do
    modalHeader "Add New Temperament"

    let nameValidation = combineDynValidations [
                pure (nonEmptyText "Name cannot be blank")
              , isNewName
          ]

    let nameForm = validatedTextEntryDyn nameValidation id

    let formContents = Temperament <$>
            formA (temperamentName =. labeledEntryA "Name" nameForm) <*>
            liftFormA (form (divisions =. labeledEntry "Divisions" nonNegativeIntEntry)) <*>
            liftFormA (form (period =. labeledEntry "Period" rationalEntry)) <*>
            pure []

    initFormA formContents initialValue
