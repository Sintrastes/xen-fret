
module XenFret.App.Widgets.ColorPicker where
import Reflex.Dom.Core
import Language.Javascript.JSaddle
import Data.Functor
import Data.Text (Text)
import Control.Monad.IO.Class
import Control.Concurrent (threadDelay)

data Color = Color {
    r :: Int,
    g :: Int,
    b :: Int
}

colorPicker :: (MonadWidget t m, _) => Color -> m (Dynamic t Color)
colorPicker initialColor = do
    elAttr "div" ("id" =: "picker") blank

    el "script" $ text "iro.ColorPicker(\"#picker\");"

    pure $ pure initialColor