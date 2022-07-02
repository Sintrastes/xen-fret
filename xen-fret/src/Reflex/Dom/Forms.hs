
module Reflex.Dom.Forms where

import Data.Profunctor
import Data.Functor.Compose
import Reflex.Dom

type Form t m a b
    = Star (Compose m (Dynamic t)) a b

type SForm t m a = Form t m a a

form :: (a -> m (Dynamic t b)) -> Form t m a b
form f = Star (Compose . f)

initForm :: Form t m a b -> a -> m (Dynamic t b)
initForm (Star f) x = getCompose $ f x

(=.) :: Profunctor f => (x -> y) -> f y a -> f x a
(=.) = lmap