
module XenFret.App.Util where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Validation
import Reflex.Dom.Core
import Reflex.Dom.Extras
import Text.Read (readMaybe)
import Data.Functor
import Data.Function
import Control.Monad
import Data.Aeson hiding (Success)
import Control.Exception
import Control.Monad.IO.Class
import XenFret.Data
import XenFret.AppData
import Data.Maybe
import Control.Lens
import Data.List (find)
import Debug.Trace
import Data.Monoid (Endo(..))

intervalListEntry :: _ => NonEmpty Int -> m (Dynamic t (Validation (NonEmpty T.Text) (NonEmpty Int)))
intervalListEntry = validatedTextEntry
    (fromEither <$> parseIntervalList)
    showIntervalList

scaleListEntry :: _ => Dynamic t Int -> NonEmpty Int -> m (Dynamic t (Validation (NonEmpty T.Text) (NonEmpty Int)))
scaleListEntry period = validatedTextEntryDyn
    ((fromEither <$>) <$> (validateSumToPeriod period `composeValidations` pure parseIntervalList))
    showIntervalList

composeValidations :: Reflex t => Dynamic t (b -> Either e c) ->  Dynamic t (a -> Either e b) -> Dynamic t (a -> Either e c)
composeValidations f g = do
    f' <- f
    g' <- g
    return $ f' <=< g'

parseIntervalList :: T.Text -> Either (NonEmpty T.Text) (NonEmpty Int)
parseIntervalList text = do
    let intervals = filter (not . T.null) $ T.splitOn " " text

    parsed <- intervals
        <&> T.unpack
        <&> readMaybe @Int
        <&> maybe (Left $ "Intervals must be whole numbers" :| [])
                Right
          & sequence

    maybe (Left $ "List of intervals cannot be empty" :| []) Right $
        NE.nonEmpty parsed

validateSumToPeriod :: Reflex t => Dynamic t Int -> Dynamic t (NonEmpty Int -> Either (NonEmpty T.Text) (NonEmpty Int))
validateSumToPeriod period' = period' <&> (\period intervals ->
    if sum intervals == period
        then return intervals
        else Left $ ("Intervals did not sum to period (" <> T.pack (show period) <> ")") :| [])

showIntervalList :: NonEmpty Int -> T.Text
showIntervalList xs = T.intercalate " " $ map (T.pack . show) $ NE.toList xs

-- | Helper function to add a scale to the given temperament.
addScale :: Temperament -> Scale -> Map T.Text [Scale] -> Map T.Text [Scale]
addScale Temperament{..} scale map = case Map.lookup _temperamentName map of
  Nothing -> Map.insert _temperamentName [scale] map
  Just scls -> Map.insert _temperamentName (scls ++ [scale]) map

getScales :: (MonadSample t m, MonadIO m) => FilePath -> m (Map T.Text [Scale])
getScales appDir = do
    appData <- loadAppData' (appDir <> "/app_data.json")
    return $ Map.fromList $ (\x -> (_temperamentName x, _scales x)) <$>
        _temperaments appData

getTunings :: (MonadSample t m, MonadIO m) => FilePath -> m (Map T.Text [Tuning])
getTunings appDir = do
    appData <- loadAppData' (appDir <> "/app_data.json")
    return $ Map.fromList $ (\x -> (_temperamentName x, _tunings x)) <$>
        _temperaments appData

scalesMap :: AppData -> Map T.Text [Scale]
scalesMap appData = Map.fromList $ (\x -> (_temperamentName x, _scales x)) <$>
        _temperaments appData

tuningsMap :: AppData -> Map T.Text [Tuning]
tuningsMap appData = Map.fromList $ (\x -> (_temperamentName x, _tunings x)) <$>
        _temperaments appData

chordsMap :: AppData -> Map T.Text [Chord]
chordsMap appData = Map.fromList $ (\x -> (_temperamentName x, _chords x)) <$>
        _temperaments appData

setTunings :: Map T.Text [Tuning] -> [Temperament] -> [Temperament]
setTunings t = mconcat (fmap (uncurry setTuningsForTemperament) (Map.toList t))

setTuningsForTemperament :: T.Text -> [Tuning] -> [Temperament] -> [Temperament]
setTuningsForTemperament name newTunings temperaments = let
     maybeTemperament = find (\x -> name == _temperamentName x) temperaments
  in case maybeTemperament of
        Just temperament -> (\x ->
            if name == _temperamentName x
                then temperament { _tunings = newTunings }
                else x) <$> temperaments
        Nothing -> temperaments

setChords :: Map T.Text [Chord] -> [Temperament] -> [Temperament]
setChords c appData = undefined

setScales :: Map T.Text [Scale] -> [Temperament] -> [Temperament]
setScales s x = trace (show x) $ trace (show $ length s) $ let
     res = mconcat (fmap (Endo . uncurry setScalesForTemperament) (Map.toList s))
            `appEndo` x
  in
     trace (show $ length x) $ trace (show $ length res) res

setScalesForTemperament :: T.Text -> [Scale] -> [Temperament] -> [Temperament]
setScalesForTemperament name newScales temperaments = let
     maybeTemperament = find (\x -> name == _temperamentName x) temperaments
  in case maybeTemperament of
        Just temperament -> (\x ->
            if name == _temperamentName x
                then temperament { _scales = newScales }
                else x) <$> temperaments
        Nothing -> temperaments

pairForms :: (Semigroup e, Reflex t) =>
    Dynamic t (Validation e a)
 -> Dynamic t (Validation e b)
 -> Dynamic t (Validation e (a, b))
pairForms x y = do
    x' <- x
    y' <- y
    pure $ (,) <$> x' <*> y'

suchThat :: (a -> Bool) -> Lens' [a] (Maybe a)
suchThat = undefined

validationToMaybe = \case
    Success x -> Just x
    Failure _ -> Nothing

selectTemperament :: _ => AppData -> Temperament -> m (Dynamic t (Maybe Temperament))
selectTemperament appData temperament = do
    let loadedTemperaments = _temperaments appData

    selectMaterial "Temperament"
        "No Temperaments Defined"
        (pure loadedTemperaments)
        (Just temperament)

getTemperaments appDir = do
    appData <- loadAppData' (appDir <> "/app_data.json")
    return $ _temperaments appData

loadAppData :: (MonadSample t m, Prerender t m, MonadIO m) => FilePath -> m AppData











loadAppData dataFile = do
    loadedData :: AppData <- liftFrontend defaultAppData $
        catch (maybe defaultAppData id <$> decodeFileStrict dataFile)
            (\(_ :: SomeException) -> return defaultAppData)
    pure loadedData


loadAppData' :: (MonadSample t m, MonadIO m) => FilePath -> m AppData











loadAppData' dataFile = liftIO $ catch (maybe defaultAppData id <$> decodeFileStrict dataFile)
    (\(_ :: SomeException) -> return defaultAppData)


persistAppData :: (ToJSON a, Applicative m, Prerender t m, Monad m ) =>
  Dynamic t a -> FilePath -> m ()








persistAppData dynAppData dataFile = do
    prerender (pure never) $ performEvent $ updated dynAppData <&>
        \newData ->
            liftIO $ encodeFile dataFile newData
    pure ()


validateNonEmpty :: T.Text -> Validation (NonEmpty T.Text) T.Text
validateNonEmpty x
    | x == ""   = Failure ("String must not be empty" :| [])
    | otherwise = Success x




