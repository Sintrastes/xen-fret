
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

type FormBuilder t m a b = StateT (FormContext t a) m b

data FormContext t a = FormContext {
    formModifiers :: [Dynamic t (a -> a)],
    initialValue  :: a
}

bind :: Reflex t => Monad m => FormContext t a -> Lens' a b -> SForm t m b -> FormBuilder t m a ()
bind ctx field subForm = do
    formValue <- lift $ initForm subForm (view field $ initialValue ctx)
    let result = set field <$> formValue
    state <- get
    put (state { formModifiers = result : formModifiers state})


buildForm :: FormBuilder t m a () -> SForm t m a
buildForm f = undefined