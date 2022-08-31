
module XenFret.App.Widgets.CRUD where
import Reflex.Dom.Core
import XenFret.AppData

crud :: _ => FilePath -> (a -> m (Dynamic t (Maybe a))) -> m [a] -> ([a] -> AppData -> AppData) -> m ()
crud appDir form getData updateData = do
    pure ()