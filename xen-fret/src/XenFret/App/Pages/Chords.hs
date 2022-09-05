
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

-- Helper "Iso" (really not a strict isomorphism)
--  to help reduce "at"s that return a list.
reduceMaybe :: Iso' (Maybe [a]) [a]
reduceMaybe = iso (fromMaybe []) Just

chordPage :: _ => FilePath -> m ()
chordPage appDir = crudPage appDir "chord" chordForm 
    (\x -> chords . at x . reduceMaybe)

chordForm :: _ =>
    AppData
 -> Temperament
 -> (T.Text -> Dynamic t [T.Text])
 -> a
 -> m (Dynamic t (Validation (NonEmpty ErrorMessage) (Temperament, a)))
chordForm appData initialTemperament currentChords initialValue = undefined