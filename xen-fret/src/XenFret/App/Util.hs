
module XenFret.App.Util where

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

