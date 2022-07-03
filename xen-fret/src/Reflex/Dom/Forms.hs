
module Reflex.Dom.Forms where

import Data.Profunctor
import Data.Functor.Compose
import Reflex.Dom
import Control.Lens
import Control.Monad.State
import Control.Applicative
import Data.Validation (Validation (..), toEither, fromEither)
import Reflex.Dom.Extras (ErrorMessage)
import qualified Data.Text as T
import Data.List.NonEmpty (NonEmpty ((:|)))

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

type AForm t m f a b
    = Star (Compose m (Compose (Dynamic t) f)) a b

type SAForm t m f a = AForm t m f a a

formA :: Functor m => (a -> m (Dynamic t (f b))) -> AForm t m f a b
formA f = Star $
    \x -> Compose $ Compose <$> f x

initFormA :: Functor m => AForm t m f a b -> a -> m (Dynamic t (f b))
initFormA (Star f) x = getCompose <$> getCompose (f x)

-- | Convert a regular form into an applicative form.
liftFormA :: (Reflex t, Applicative f, Functor m) => Form t m a b -> AForm t m f a b
liftFormA (Star f) = Star (\a -> let
    Compose x = f a
    y = Compose . (pure <$>) <$> x
 in Compose y)

nonEmptyText :: ErrorMessage -> T.Text -> Validation (NonEmpty ErrorMessage) T.Text
nonEmptyText msg x =
    if x == ""
        then Failure $ msg :| []
        else Success x

validateNonNull :: ErrorMessage -> Maybe a -> Validation (NonEmpty ErrorMessage) a
validateNonNull msg = \case
  Nothing -> Failure $ msg :| []
  Just x  -> Success x

combineDynValidations :: forall e a t. _ => [Dynamic t (a -> Validation e a)] -> Dynamic t (a -> Validation e a)
combineDynValidations = from . foldr combine (pure return) . fmap to
  where
    to :: Dynamic t (a -> Validation e a) -> Dynamic t (a -> Either e a)
    to   = fmap (fmap toEither)

    from :: Dynamic t (a -> Either e a) -> Dynamic t (a -> Validation e a)
    from = fmap (fmap fromEither)

    combine :: Dynamic t (a -> Either e a) -> Dynamic t (a -> Either e a) -> Dynamic t (a -> Either e a)
    combine x y = do
        x' <- x
        y' <- y
        pure $ x' >=> y'

