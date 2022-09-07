
module XenFret.App.Widgets.ColorPicker where
import Reflex.Dom.Core
import Language.Javascript.JSaddle
import Data.Functor
import Data.Text (Text)
import Control.Monad.IO.Class
import Control.Concurrent (threadDelay)
import Data.Aeson.TH

data Color = Color {
    r :: Int,
    g :: Int,
    b :: Int
}

$(deriveJSON defaultOptions ''Color)

colorPicker :: (MonadWidget t m, _) => Text -> Color -> m (Dynamic t Color)
colorPicker ident initialColor = do
    elAttr "div" ("id" =: ("picker-" <> ident)) blank

    el "script" $ text $ 
        "var colorPicker = iro.ColorPicker(\"#picker-" <> ident <> "\", { layout: [{component: iro.ui.Box}, {component: iro.ui.Slider, options: {sliderType: 'hue'}}] });"

    onClick <- button "test"

    performEvent $ onClick <&> \_ -> do
        currentColor <- liftJSM $ valToStr =<< eval ("colorPicker.color.hexString" :: Text)
        liftIO $ print currentColor
        pure ()

    pure $ pure initialColor