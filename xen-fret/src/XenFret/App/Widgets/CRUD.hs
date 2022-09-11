{-# LANGUAGE TupleSections #-}

module XenFret.App.Widgets.CRUD where

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.List.NonEmpty (NonEmpty(..))
import Data.Validation
import XenFret.Data
import XenFret.AppData
import XenFret.App.Util
import Reflex.Dom.Core hiding(Home, button, checkbox)
import Reflex.Dom.Extras
import qualified Data.Sequence as Seq
import Data.List
import Data.Maybe hiding (mapMaybe)
import Data.Functor
import Control.Monad
import Data.Foldable
import Control.Applicative
import Control.Lens (toListOf, Lens', (^.), (%~), At (at), (.=), lens)
import Control.Lens.Prism
import Diagrams (yDir)
import Control.Monad.Fix
import Data.Default
import Control.Monad.IO.Class

-- | Generic widget for a CRUD page that updates a collection contained in AppData
-- depending on a temperament.
crudPage :: (Named a, Default a, Eq a, Show a, MonadWidget t m, Prerender t m) =>
    FilePath
 -> T.Text
 -> (AppData -> Temperament -> (T.Text -> Dynamic t [T.Text]) -> a -> m (Dynamic t (Validation (NonEmpty ErrorMessage) (Temperament, a))))
 -> (T.Text -> Lens' AppData [a])
 -> m ()
crudPage appDir entityName form optic = mdo
    appData <- loadAppData (appDir <> "/app_data.json")

    newEntityClick <- button $ "New " <> entityName

    addedEntity <- switch . current <$> prerender (pure never) (performEvent $ newEntitySubmitted <&> \case
        Nothing -> pure appData
        Just (temperament, entity) -> do
            toast $ "Added new " <> entityName
            pure $ addEntity (optic (_temperamentName temperament)) appData entity)

    let updatedEntities = leftmost [addedEntity, removedEntity, editedEntity]

    dynUpdatedData <- holdDyn appData
        updatedEntities

    let getName currentName = \temperament -> dynUpdatedData <&> (\x ->
         let ts = _temperaments x
             n = find ((== temperament) . _temperamentName) ts
             ents = maybe [] (\y -> getEntitiesFor (_temperamentName y) optic x) n
             curr = currentName
         in filter ((/= curr) . Just) $ fmap name ents)

    let dynEntities = dynUpdatedData <&> getEntitiesMap optic

    newEntitySubmitted <- validatedModal newEntityClick $ \_ ->
        form appData (head $ _temperaments appData) (getName Nothing) def

    persistAppData dynUpdatedData
        (appDir <> "/app_data.json")

    (editEvents, deleteEvents) <- fanEither <$> (switchHold never =<< dyn (dynEntities <&> \currentEntities ->
        elClass "ul" "collection" $ do
            itemEvents <- forM (Map.toList currentEntities) $ \(temperamentName, scales) -> do
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

    let removedEntity = pushAlways (\(temperamentName, deletedEntity) -> do
            currentData <- sample $ current dynUpdatedData

            pure $ deleteEntity (optic temperamentName) currentData deletedEntity) deleteEvents

    completeEditDialog <- validatedModal editEvents $ \(temperament, entity) -> do
        res <- form appData
            (fromJust $ find (\x -> _temperamentName x == temperament) $ _temperaments appData)
            (getName (Just $ name entity)) entity
        pure $ fmap (\(x, y) -> (x, entity, y)) <$> res

    let editSubmitted = mapMaybe id completeEditDialog

    let editedEntity = pushAlways (\(temperament, origEntity, newEntity) -> do
            currentData <- sample $ current dynUpdatedData

            pure $ editEntity (optic $ _temperamentName temperament) currentData origEntity newEntity) editSubmitted

    blank

addEntity :: Lens' AppData [a] -> AppData -> a -> AppData
addEntity optic appData newEntity = appData & optic %~ (++ [newEntity])

editEntity :: Eq a => Lens' AppData [a] -> AppData -> a -> a -> AppData
editEntity optic appData old new = appData & optic . atElem old . _Just .~ new

atElem :: Eq a => a -> Lens' [a] (Maybe a)
atElem elem = lens (find (== elem)) (`replaceAt` elem)
    where
      replaceAt [] _ _ = []
      replaceAt xs _ Nothing = xs
      replaceAt (x : xs) elem (Just y)
          | x == elem = y : replaceAt xs elem (Just y)
          | otherwise = x : replaceAt xs elem (Just y)

deleteEntity :: Eq a => Lens' AppData [a] -> AppData -> a -> AppData
deleteEntity optic appData x = appData & optic %~ filter (/= x)

getEntitiesFor :: t -> (t -> Lens' s a) -> s -> a
getEntitiesFor temperament optic appData = appData ^. optic temperament

getEntities :: (T.Text -> Lens' AppData [a]) -> AppData -> [a]
getEntities optic appData = mconcat $ _temperaments appData <&> \temperament ->
    appData ^. optic (_temperamentName temperament)

getEntitiesMap :: (T.Text -> Lens' AppData [a]) -> AppData -> Map.Map T.Text [a]
getEntitiesMap optic appData = Map.fromList $ _temperaments appData <&> \temperament ->
    (_temperamentName temperament, appData ^. optic (_temperamentName temperament))

