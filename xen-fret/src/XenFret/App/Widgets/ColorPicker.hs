
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
import Numeric (showIntAtBase, showHex, readHex)
import Data.Char (intToDigit)
import Text.ParserCombinators.ReadP
import Data.Maybe (fromJust)

data Color = Color {
    r :: Int,
    g :: Int,
    b :: Int
}

color2hex :: Color -> String
color2hex (Color r g b) = "#" <> showHex2 r <> showHex2 g <> showHex2 b

showHex2 :: Int -> String
showHex2 x = let
    hex = showHex x ""
  in if length hex < 2
        then "0" <> hex
        else hex

hex2color :: String -> Maybe Color
hex2color [_, r1, r2, g1, g2, b1, b2] = let
    r = fst $ head $ readHex [r1, r2]
    g = fst $ head $ readHex [g1, g2]
    b = fst $ head $ readHex [b1, b2]
  in Just $ Color r g b
hex2color _ = Nothing

$(deriveJSON defaultOptions ''Color)

colorPicker :: (MonadWidget t m, _) => Text -> Color -> m (Dynamic t Color)
colorPicker ident initialColor = do
    postBuild <- delay 0.1 =<< getPostBuild
    (colorUpdate, onUpdateColor) <- newTriggerEvent

    _ <- elAttr "div" ("id" =: ("picker-" <> ident)) $ do
        performEvent $ postBuild <&> \_ ->
            liftJSM $ do
                picker <- eval ("iro.ColorPicker(\"#picker-" <> ident <> "\", { layout: " <> 
                            "[{component: iro.ui.Box}, " <>
                            "{component: iro.ui.Slider, options: {sliderType: 'hue'}}]" <>
                       "});" :: T.Text)
                _ <- liftJSM $ jsg2 ("bindColorPicker" :: T.Text) picker
                    (fun $ \_ _ args -> do
                        let firstArg = head args
                        hexString <- textFromJSString <$> valToStr firstArg
                        liftIO $ onUpdateColor $ fromJust $ hex2color $ T.unpack hexString
                        liftIO $ putStrLn $ T.unpack hexString)
                pure ()

    holdDyn initialColor
        colorUpdate 