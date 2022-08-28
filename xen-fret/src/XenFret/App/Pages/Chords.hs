
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

chordPage appDir = do
    el "p" $ text "chords"