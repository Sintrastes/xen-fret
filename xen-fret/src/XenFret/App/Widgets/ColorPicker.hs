
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

    el "script" $ text "var colorPicker = iro.ColorPicker(\"#picker\", { layout: [{component: iro.ui.Box}, {component: iro.ui.Slider, options: {sliderType: 'hue'}}] });"

    onClick <- button "test"

    performEvent $ onClick <&> \_ -> do
        currentColor <- liftJSM $ valToStr =<< eval ("colorPicker.color.hexString" :: Text)
        liftIO $ print currentColor
        pure ()

    pure $ pure initialColor