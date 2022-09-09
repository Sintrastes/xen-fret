
module XenFret.App.Widgets.ColorPicker where
import Reflex.Dom.Core
import Language.Javascript.JSaddle ( liftJSM, js, jsg2, fun, textFromJSString, valToStr, jsg, eval, js1 )
import Data.Functor
import Data.Text (Text)
import Control.Monad.IO.Class
import Control.Concurrent (threadDelay)
import Data.Aeson.TH
import qualified Data.Text as T
import Control.Lens ((^.))
import Reflex.Dom.Extras (liftFrontend, liftFrontend')

data Color = Color {
    r :: Int,
    g :: Int,
    b :: Int
}

color2hex :: Color -> String
color2hex (Color r g b) = undefined

hex2color :: String -> Maybe Color
hex2color = undefined

$(deriveJSON defaultOptions ''Color)

colorPicker :: (MonadWidget t m, _) => Text -> Color -> m (Dynamic t Color)
colorPicker ident initialColor = do
    postBuild <- delay 0.1 =<< getPostBuild
    (colorUpdate, onUpdateColor) <- newTriggerEvent

    elAttr "div" ("id" =: ("picker-" <> ident)) $ do
        performEvent $ postBuild <&> \_ ->
            liftJSM $ do
                colorPicker <- eval ("iro.ColorPicker(\"#picker-" <> ident <> "\", { layout: [{component: iro.ui.Box}, {component: iro.ui.Slider, options: {sliderType: 'hue'}}] });" :: T.Text)
                liftJSM $ jsg2 ("bindColorPicker" :: T.Text) colorPicker
                    (fun $ \_ this args -> do
                        let firstArg = args !! 0
                        hexString <- textFromJSString <$> valToStr firstArg
                        -- onUpdateColor $ hex2color hexString
                        liftIO $ putStrLn $ T.unpack hexString)
                pure ()

    pure $ pure initialColor