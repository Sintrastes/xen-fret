
module XenFret.App.Pages.Scales where

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

scalePage :: _ => FilePath -> m ()
scalePage appDir = mdo
    appData <- loadAppData (appDir <> "/app_data.json")
    currentScales <- getScales (appDir <> "/app_data.json")

    newScaleClick <- button "New Scale"

    addedScale <- switch . current <$> prerender (pure never) (performEvent $ newScaleSubmitted <&> \case
        Nothing -> pure currentScales
        Just (temperament, scale) -> do
            toast "Added new temperament"
            currentScales <- getScales appDir
            pure (addScale temperament scale currentScales))

    let updatedScales = leftmost [addedScale, removedScale, editedScale]

    dynScales <- holdDyn currentScales
        updatedScales

    -- Dynamic check for whether or not a scale's name already exists in the list of scales for a given temperament.
    let isNewName initialName = dynScales <&> \scales temperament name -> 
          let names = fmap scaleName (fromMaybe [] $ Map.lookup (temperamentName temperament) scales) in
            if name `elem` names && (Just name /= initialName)
                then Failure $ "There is already a scale with this name." :| []
                    else Success name

    newScaleSubmitted <- validatedModal newScaleClick $ \_ ->
        scaleForm appData (head $ _temperaments appData) (isNewName Nothing) def

    let dynAppData = dynScales <&> \s ->
            appData { _temperaments = setScales s (_temperaments appData) }

    persistAppData dynAppData
        (appDir <> "/app_data.json")

    (editEvents, deleteEvents) <- fanEither <$> (switchHold never =<< dyn (dynScales <&> \currentScales ->
        elClass "ul" "collection" $ do
            itemEvents <- forM (Map.toList currentScales) $ \(temperamentName, scales) -> do
                el "h3" $ text temperamentName
                forM scales $ \scale -> do
                    elClass "li" "collection-item" $ do
                        deleteEvent <- domEvent Click . fst <$> elClass' "i" "material-icons" (
                            text "clear")
                        editEvent <- domEvent Click . fst <$> elClass' "i" "material-icons"
                            (text "edit")
                        el "span" $ text $
                            T.pack $ show scale
                        pure $ leftmost 
                            [
                              deleteEvent $> Right (temperamentName, scale)
                            , editEvent $> Left (temperamentName, scale)
                            ]
            pure (leftmost $ join itemEvents)))

    let removedScale = pushAlways (\(temperamentName, deletedScale) -> do
            currentScales <- getScales appDir
            let scales = currentScales Map.! temperamentName
            let updatedScales = Map.insert temperamentName
                    (filter (/= deletedScale) scales) currentScales

            pure updatedScales) deleteEvents

    completeEditDialog <- validatedModal editEvents $ \(temperament, scale) -> do
        res <- scaleForm appData
            (fromJust $ find (\x -> temperamentName x == temperament) $ _temperaments appData)
            (isNewName (Just $ scaleName scale)) scale
        pure $ fmap (\(x,y) -> (x, scale, y)) <$> res

    let editSubmitted = mapMaybe id completeEditDialog

    let editedScale = pushAlways (\(temperament, origScale, newScale) -> do
            currentScales <- getScales appDir
            let scales = currentScales Map.! temperamentName temperament
            let indexOfScale = fromJust $ scales & elemIndex origScale
            let updatedScales = Seq.fromList scales
                    & Seq.update indexOfScale newScale
                    & toList

            let updatedMap = Map.insert 
                    (temperamentName temperament)
                    updatedScales currentScales

            pure updatedMap) editSubmitted

    blank

scaleForm :: MonadWidget t m =>
     AppData
  -> Temperament
  -> Dynamic t (Temperament -> T.Text -> Validation (NonEmpty ErrorMessage) T.Text)
  -> Scale
  -> m (Dynamic t (Validation (NonEmpty ErrorMessage) (Temperament, Scale)))
scaleForm appData initialTemperament isValidName initialValue = mdo
    modalHeader "Add New Scale"

    currentTemperament <- holdDyn initialTemperament =<<
        delay 0.1 temperamentUpdated

    let scaleValidation = combineDynValidations
          [ pure $ nonEmptyText "Scale name must not be entry"
          , isValidName <*> currentTemperament]

    temperamentForm <- (validateNonNull "Temperament must be selected" <$>) <$>
            selectTemperament appData initialTemperament

    let temperamentUpdated = fmapMaybe validationToMaybe
            (updated temperamentForm)

    let periodDyn = currentTemperament <&> divisions

    scaleForm <- initFormA (Scale <$>
          formA (scaleName =. labeledEntryA "Name"
            (validatedTextEntryDyn scaleValidation id)) <*>
          formA (scaleIntervals =. labeledEntryA "Intervals"
            (scaleListEntry periodDyn))) initialValue

    pure $ pairForms temperamentForm scaleForm