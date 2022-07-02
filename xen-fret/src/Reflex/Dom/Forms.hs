
module Reflex.Dom.Forms where

import Data.Profunctor
import Data.Functor.Compose
import Reflex.Dom
import Control.Lens
import Control.Monad.State
import Control.Applicative

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


buildForm :: Reflex t => Monad m => FormBuilder t m a () -> SForm t m a
buildForm f = form $ \initialValue -> let
    initialContext = FormContext [] initialValue
  in do
    (_, finalContext) <- runStateT f initialContext
    let modifiers = formModifiers finalContext
    let modifier = foldr (liftA2 (.)) (pure id) modifiers

    return $ 
        modifier <*> pure initialValue