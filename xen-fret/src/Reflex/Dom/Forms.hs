
module Reflex.Dom.Forms where

import Data.Profunctor
import Data.Functor.Compose
import Reflex.Dom
import Control.Lens
import Control.Monad.State

type Form t m a b
    = Star (Compose m (Dynamic t)) a b

type SForm t m a = Form t m a a

form :: (a -> m (Dynamic t b)) -> Form t m a b
form f = Star (Compose . f)

initForm :: Form t m a b -> a -> m (Dynamic t b)
initForm (Star f) x = getCompose $ f x

(=.) :: Profunctor f => (x -> y) -> f y a -> f x a
(=.) = lmap

data SomeLens a = forall b. SomeLens (Lens' a b)

type FormBuilder t m a = StateT [SomeLens a] m a

buildForm :: FormBuilder t m a -> SForm t m a
buildForm f = undefined