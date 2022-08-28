
module XenFret.App.Widgets.Github where

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.List.NonEmpty (NonEmpty)
import Data.Validation
import qualified Data.List.NonEmpty as NE
import XenFret.App.Widgets.Fretboard
import Reflex.Dom.Core hiding(Home, button, checkbox)
import Reflex.Dom.Extras
import Data.Aeson
import Data.Aeson.Casing
import GHC.Generics
import Data.Functor

-- | Json object for a github api response.
data GithubData = GithubData {
    stargazersCount :: Int,
    forksCount :: Int
} deriving(Generic)

instance ToJSON GithubData where
   toJSON = genericToJSON $ (aesonDrop 0 snakeCase)
instance FromJSON GithubData where
   parseJSON = genericParseJSON $ (aesonDrop 0 snakeCase)

repoUrl :: T.Text
repoUrl = "https://github.com/sintrastes/xen-fret"

repoApiUrl :: T.Text
repoApiUrl = "https://api.github.com/repos/sintrastes/xen-fret"

fetchGithubData :: _ => Event t () -> m (Event t (Maybe GithubData))
fetchGithubData fetchEv = getAndDecode
    (fetchEv $> repoApiUrl)

-- | Widget used to display source information.
-- adapted from material for mkdocs (https://squidfunk.github.io/mkdocs-material/),
-- (MIT licensed)
githubWidget :: _ => m ()
githubWidget = do
    dataFetched <- fetchGithubData =<< getPostBuild

    -- Build up Dyns for our data
    starsDynText <- foldDyn
        (\ghData curr ->
            maybe "ERR" (T.pack . show . stargazersCount) ghData)
        "" dataFetched

    forksDynText <- foldDyn
        (\ghData curr ->
            maybe "ERR" (T.pack . show . forksCount) ghData)
        "" dataFetched

    -- Build the UI
    elAttr "a" ("style" =: "margin-top: -5.5px;" <> "class" =: "right" <> "href" =: repoUrl) $ elAttr "div" ("style" =: "display: block; margin-right: 25px;") $ do
        elAttr "div" ("style" =: "height: 2.4rem;width: 2rem;display: inline-block;vertical-align: middle;") $ gitIcon
        elAttr "div" ("style" =: "vertical-align: top; display: inline-flex;flex-direction: column;align-items: center;justify-content: center;margin-left: -2rem;padding-left: 2.4rem;font-size: .65rem;") $ do
            elAttr "div" ("style" =: "height: 1.5em;") $ text "sintrastes/xen-fret"
            elAttr "div" ("style" =: "height: 1.5em;") $ do
                starsIcon
                stars starsDynText
                forksIcon
                forks forksDynText
  where
    gitIcon :: _ => m ()
    gitIcon = elSvg "svg" ("viewBox" =: "0 0 448 512" <> "xmlns" =: "http://www.w3.org/2000/svg") $
        elSvg "path" ("d" =: "M439.55 236.05 244 40.45a28.87 28.87 0 0 0-40.81 0l-40.66 40.63 51.52 51.52c27.06-9.14 52.68 16.77 43.39 43.68l49.66 49.66c34.23-11.8 61.18 31 35.47 56.69-26.49 26.49-70.21-2.87-56-37.34L240.22 199v121.85c25.3 12.54 22.26 41.85 9.08 55a34.34 34.34 0 0 1-48.55 0c-17.57-17.6-11.07-46.91 11.25-56v-123c-20.8-8.51-24.6-30.74-18.64-45L142.57 101 8.45 235.14a28.86 28.86 0 0 0 0 40.81l195.61 195.6a28.86 28.86 0 0 0 40.8 0l194.69-194.69a28.86 28.86 0 0 0 0-40.81z") blank

    stars starsDynText = dynText starsDynText {- octicons/star-16.svg -}
    forks forksDynText = dynText forksDynText {- octicons/repo-forked-16.svg -}

    forksIcon :: _ => m ()
    forksIcon = elSvg "svg" ("style" =: "margin-left: 0.4rem; height: 0.6rem; width: 0.6rem;" <>"viewBox" =: "0 0 16 16" <> "xmlns" =: "http://www.w3.org/2000/svg") $
        elSvg "path" ("fill-rule" =: "evenodd" <> "d" =: "M5 3.25a.75.75 0 1 1-1.5 0 .75.75 0 0 1 1.5 0zm0 2.122a2.25 2.25 0 1 0-1.5 0v.878A2.25 2.25 0 0 0 5.75 8.5h1.5v2.128a2.251 2.251 0 1 0 1.5 0V8.5h1.5a2.25 2.25 0 0 0 2.25-2.25v-.878a2.25 2.25 0 1 0-1.5 0v.878a.75.75 0 0 1-.75.75h-4.5A.75.75 0 0 1 5 6.25v-.878zm3.75 7.378a.75.75 0 1 1-1.5 0 .75.75 0 0 1 1.5 0zm3-8.75a.75.75 0 1 0 0-1.5.75.75 0 0 0 0 1.5z") blank


    starsIcon :: _ => m ()
    starsIcon = elSvg "svg" ("style" =: "margin-left: 0.4rem; height: 0.6rem; width: 0.6rem;" <>"viewBox" =: "0 0 16 16" <> "xmlns" =: "http://www.w3.org/2000/svg") $
        elSvg "path" ("fill-rule" =: "evenodd" <> "d" =: "M8 .25a.75.75 0 0 1 .673.418l1.882 3.815 4.21.612a.75.75 0 0 1 .416 1.279l-3.046 2.97.719 4.192a.75.75 0 0 1-1.088.791L8 12.347l-3.766 1.98a.75.75 0 0 1-1.088-.79l.72-4.194L.818 6.374a.75.75 0 0 1 .416-1.28l4.21-.611L7.327.668A.75.75 0 0 1 8 .25zm0 2.445L6.615 5.5a.75.75 0 0 1-.564.41l-3.097.45 2.24 2.184a.75.75 0 0 1 .216.664l-.528 3.084 2.769-1.456a.75.75 0 0 1 .698 0l2.77 1.456-.53-3.084a.75.75 0 0 1 .216-.664l2.24-2.183-3.096-.45a.75.75 0 0 1-.564-.41L8 2.694v.001z") blank
