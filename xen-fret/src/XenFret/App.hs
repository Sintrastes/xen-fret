{-# OPTIONS_GHC -Wno-name-shadowing #-}

module XenFret.App where

import Data.Maybe (isJust, isNothing, fromJust, fromMaybe)
import Control.Monad
import XenFret.Util
import XenFret
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Reflex.Dom.Core hiding(Home, button, checkbox)
import Reflex.Dom.Extras
import qualified Data.Text as T
import Data.Functor
import qualified Data.Map as Map
import qualified Data.List.NonEmpty as NE
import Control.Exception
import System.Info
import System.Directory
import Data.List hiding (transpose)
import Data.Aeson hiding (Success)
import Control.Monad.IO.Class
import GHC.Float
import XenFret.Data
import XenFret.AppData
import GHC.Generics
import qualified Language.Javascript.JSaddle as JS
import Data.ByteString.Lazy (toStrict)
import Debug.Trace (traceIO, trace)
import Reflex.Dom.Extras
import Reflex.Dom.Forms
import Data.Validation
import XenFret.App.Util
import Control.Monad.Fix
import Data.Map (Map, lookup, insert)
import Data.Foldable

import XenFret.App.Widgets.Github
import XenFret.App.Widgets.Fretboard

import XenFret.App.Pages.Main
import XenFret.App.Pages.Chords
import XenFret.App.Pages.Preferences
import XenFret.App.Pages.Scales
import XenFret.App.Pages.Temperaments
import XenFret.App.Pages.Tunings

header :: _ => m ()
header = do
  el "title" $ text "Xen Fret"
  elAttr "meta" (
    "name" =: "viewport" <>
    "content" =: "width=device-width, initial-scale=1") blank
  elAttr "script" (
    "src" =: "https://sintrastes.github.io/demos/xen_fret/main.js") blank
  elAttr "script" (
    "src" =: "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/js/materialize.min.js") blank
  elAttr "link" (
    "id" =: "css-style" <>
    "href" =: "https://sintrastes.github.io/demos/montague/materialize.min.css" <>
    "type" =: "text/css" <>
    "rel" =: "stylesheet") blank
  elAttr "link" (
    "href" =: "https://sintrastes.github.io/demos/xen_fret/main.css" <>
    "type" =: "text/css" <>
    "rel" =: "stylesheet") blank
  elAttr "link" (
    "href" =: "https://sintrastes.github.io/demos/montague/main.css" <>
    "type" =: "text/css" <>
    "rel" =: "stylesheet") blank
  elAttr "link" (
    "href" =: "https://fonts.googleapis.com/icon?family=Material+Icons" <>
    "type" =: "text/css" <>
    "rel" =: "stylesheet") blank
  elAttr "link" (
    "href" =: "https://sintrastes.github.io/demos/montague/w3.css" <>
    "type" =: "text/css" <>
    "rel" =: "stylesheet") blank
  elAttr "link" (
    "id" =: "material-colors" <>
    "href" =: "https://sintrastes.github.io/demos/montague/material-colors-default.css" <>
    "type" =: "text/css" <>
    "rel" =: "stylesheet") blank

data Pages =
    Home
  | Preferences
  | Temperaments
  | Tunings
  | Scales
  | EditScale
  | EditTemperament
  | Chords
    deriving(Show)

app :: _ => m ()
app = do
    -- Setup the application directory.
    appDir <- if "android" `isInfixOf` os
        then pure "/data/data/org.xenfret.app"
        else liftFrontend "/" getHomeDirectory <&> (<> "/.xenfret")

    _ <- liftFrontend (Right ()) $ catch
        (do createDirectoryIfMissing True appDir
            pure $ Right ())
        (\(e :: SomeException) -> pure $ Left e)

    navEvents <- materialNavBar [Home, Temperaments, Tunings, Scales, Chords, Preferences] $
        githubWidget

    currentPage <- holdDyn Home navEvents

    _ <- dyn $ currentPage <&> \case
        Home -> mainPage appDir
        Temperaments -> temperamentPage appDir
        Tunings -> tuningPage appDir
        Scales -> scalePage appDir
        Preferences -> preferencePage appDir
        Chords ->  blank -- chordPage appDir
    blank



