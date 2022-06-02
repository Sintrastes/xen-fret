module Main where

import App
import Language.Javascript.JSaddle.Warp
import Reflex.Dom.Core

main :: IO ()
main = run 3911 $ mainWidgetWithHead header app
