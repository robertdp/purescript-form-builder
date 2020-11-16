module Data.UI.Validation where

import Prelude
import Data.Lens (Lens', Prism', is, lens, prism', view)
import Data.Maybe (Maybe(..))
import Data.Traversable (class Foldable, class Traversable, traverseDefault)

data Validated a
  = Fresh a
  | Modified a

data Result a
  = Valid a
  | Invalid

unvalidated :: forall a. Validated a -> a
unvalidated = case _ of
  Fresh a -> a
  Modified a -> a

_Validated :: forall a. Lens' (Validated a) a
_Validated = lens get set
  where
  get = case _ of
    Fresh a -> a
    Modified a -> a

  set = case _ of
    Fresh _ -> Fresh
    Modified _ -> Modified

_Fresh :: forall a. Prism' (Validated a) a
_Fresh =
  prism' Fresh case _ of
    Fresh a -> Just a
    Modified _ -> Nothing

_Modified :: forall a. Prism' (Validated a) a
_Modified =
  prism' Modified case _ of
    Fresh _ -> Nothing
    Modified a -> Just a

isFresh :: forall a. Validated a -> Boolean
isFresh = is _Fresh

isModified :: forall a. Validated a -> Boolean
isModified = is _Modified

_Valid :: forall a. Prism' (Result a) a
_Valid =
  prism' Valid case _ of
    Valid a -> Just a
    Invalid -> Nothing

_Invalid :: forall a. Prism' (Result a) Unit
_Invalid =
  prism' (const Invalid) case _ of
    Valid _ -> Nothing
    Invalid -> Just unit

isValid :: forall a. Result a -> Boolean
isValid = is _Valid

isInvalid :: forall a. Result a -> Boolean
isInvalid = is _Invalid

derive instance functorValidated :: Functor Validated

instance applyValidated :: Apply Validated where
  apply (Fresh f) (Fresh a) = Fresh (f a)
  apply f a = Modified ((unvalidated f) (unvalidated a))

instance applicativeValidated :: Applicative Validated where
  pure = Fresh

derive instance functorResult :: Functor Result

instance applyResult :: Apply Result where
  apply (Valid f) (Valid a) = Valid (f a)
  apply _ _ = Invalid

instance applicativeResult :: Applicative Result where
  pure = Valid

instance foldableResult :: Foldable Result where
  foldr f b (Valid a) = f a b
  foldr _ b Invalid = b
  foldl f b (Valid a) = f b a
  foldl _ b Invalid = b
  foldMap f (Valid a) = f a
  foldMap _ Invalid = mempty

instance traversableResult :: Traversable Result where
  traverse = traverseDefault
  sequence (Valid m) = Valid <$> m
  sequence Invalid = pure Invalid

class CanValidate u v | u -> v where
  fresh :: Prism' (Validated v) u
  modified :: Prism' (Validated v) u
  fromValidated :: Validated v -> u

instance canValidateValidated :: CanValidate (Validated a) a where
  fresh = identity
  modified = identity
  fromValidated = identity
else instance canValidateAny :: CanValidate a a where
  fresh = _Fresh
  modified = _Modified
  fromValidated = view _Validated
