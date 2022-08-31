
module XenFret.App.Widgets.CRUD where
import Reflex.Dom.Core
import XenFret.AppData
import Witherable(Witherable, wither)
import Control.Applicative

type Wither s t a b = forall f. Alternative f => (a -> f b) -> s -> f t
type Wither' s a = Wither s s a a

withered :: (Alternative f, Witherable t) => (a -> f b) -> t a -> f (t b)
withered f = wither (optional . f)

-- | Generic widget for a CRUD page that updates a collection contained in AppData.
crud :: _ => FilePath -> (a -> m (Dynamic t (Maybe a))) -> Wither' AppData a -> m ()
crud appDir form optic = do
    pure ()