
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

intervalListEntry :: _ => NonEmpty Int -> m (Dynamic t (Validation (NonEmpty T.Text) (NonEmpty Int)))
intervalListEntry = validatedTextEntry
    (fromEither <$> parseIntervalList)
    showIntervalList

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

showIntervalList :: NonEmpty Int -> T.Text
showIntervalList xs = T.intercalate " " $ map (T.pack . show) $ NE.toList xs 

