module Data.UI where

import Prelude
import Data.Lens (Lens', over, view)
import Effect (Effect)

class FocusState m where
  focus :: forall state state'. Lens' state state' -> m state' ~> m state

newtype UI ui props state a
  = UI (props -> state -> { render :: ((state -> state) -> Effect Unit) -> ui, return :: a })

runUI ::
  forall a props state ui.
  UI ui props state a ->
  props ->
  state ->
  { render :: ((state -> state) -> Effect Unit) -> ui
  , return :: a
  }
runUI (UI run) = run

ui ::
  forall a props state ui.
  (props -> state -> { render :: ((state -> state) -> Effect Unit) -> ui, return :: a }) ->
  UI ui props state a
ui = UI

liftUI :: forall state props ui. ui -> UI ui props state Unit
liftUI vdom = ui \_ _ -> { render: \_ -> vdom, return: unit }

withUI :: forall state props a ui' ui. (ui -> ui') -> UI ui props state a -> UI ui' props state a
withUI f ui' =
  ui \props state -> case runUI ui' props state of
    { render, return } -> { render: \onChange -> f (render onChange), return }

withProps :: forall state props ui a. (props -> UI ui props state a) -> UI ui props state a
withProps f = ui \props state -> case f props of UI run -> run props state

withState :: forall state props ui a. (state -> UI ui props state a) -> UI ui props state a
withState f = ui \props state -> case f state of UI run -> run props state

instance functorUI :: Functor (UI ui props state) where
  map f (UI run) = UI \props state -> case run props state of { render, return } -> { render, return: f return }

instance applyUI :: Monoid ui => Apply (UI ui props state) where
  apply = ap

instance applicativeUI :: Monoid ui => Applicative (UI ui props state) where
  pure a = UI \_ _ -> { render: \_ -> mempty, return: a }

instance bindUI :: Monoid ui => Bind (UI ui props state) where
  bind (UI run) f =
    UI \props state -> case run props state of
      a -> case f a.return of
        UI run' -> case run' props state of
          b -> { render: \setState -> a.render setState <> b.render setState, return: b.return }

instance monadUI :: Monoid ui => Monad (UI ui props state)

instance semigroupUI :: (Monoid ui, Semigroup a) => Semigroup (UI ui props state a) where
  append (UI runA) (UI runB) =
    UI \props state -> case runA props state, runB props state of
      a, b -> { render: \setState -> a.render setState <> b.render setState, return: a.return <> b.return }

instance monoidUI :: (Monoid ui, Monoid a) => Monoid (UI ui props state a) where
  mempty = UI \_ _ -> { render: \_ -> mempty, return: mempty }

instance focusStateUI :: FocusState (UI ui props) where
  focus optic (UI run) =
    UI \props state -> case run props (view optic state) of
      { render, return } -> { render: \setState -> render \f -> setState (over optic f), return }
