module Data.UI.Form where

import Prelude
import Data.UI (class FocusState, UI, focus, runUI, ui)
import Data.UI.Form.Virtual as Virtual
import Data.UI.Validation (class CanValidate, Result(..), Validated, fromValidated, isModified, modified)
import Control.Alt (class Alt)
import Control.Alternative (class Alternative, class Plus)
import Control.Apply (lift2)
import Data.Array (unsnoc)
import Data.Either (Either(..))
import Data.Lens (review)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Effect (Effect)
import React.Basic (JSX)
import React.Basic as React

newtype Form props state a
  = Form (UI Virtual.Tree props state (Result a))

newtype SeqForm props state a
  = SeqForm (UI Virtual.Tree props state (Result a))

runForm :: forall a props state. Form props state a -> props -> state -> { render :: ((state -> state) -> Effect Unit) -> Virtual.Tree, return :: Result a }
runForm (Form run) = runUI run

form :: forall props a state. (props -> state -> { render :: ((state -> state) -> Effect Unit) -> Virtual.Tree, return :: Result a }) -> Form props state a
form = Form <<< ui

form_ :: forall state props. (props -> state -> ((state -> state) -> Effect Unit) -> JSX) -> Form props state state
form_ render =
  form \props state ->
    { render: \setState -> [ Virtual.Child { key: Nothing, child: render props state setState } ]
    , return: pure state
    }

child :: forall props state. JSX -> Form props state Unit
child jsx = form \_ _ -> { render: \_ -> [ Virtual.Child { key: Nothing, child: jsx } ], return: pure unit }

withProps :: forall a props state. (props -> Form props state a) -> Form props state a
withProps f = form \props state -> runForm (f props) props state

withState :: forall a props state. (state -> Form props state a) -> Form props state a
withState f = form \props state -> runForm (f state) props state

withForm :: forall a props state. (Virtual.Tree -> Virtual.Tree) -> Form props state a -> Form props state a
withForm f form' = form \props state -> case runForm form' props state of { render, return } -> { render: \setState -> f (render setState), return }

wrap :: forall a props state. (Array JSX -> JSX) -> Form props state a -> Form props state a
wrap f = withForm \children -> [ Virtual.Wrapper { key: Nothing, wrap: f, children } ]

sequential :: forall props a state. String -> Form props state a -> SeqForm props state a
sequential key (Form run) =
  SeqForm
    $ ui \props state -> case runUI run props state of
        { render, return } ->
          { render:
              \setState ->
                [ Virtual.Wrapper
                    { key: Just key
                    , wrap: React.keyed key <<< React.fragment
                    , children: render setState
                    }
                ]
          , return
          }

parallel :: forall props a state. String -> SeqForm props state a -> Form props state a
parallel key (SeqForm run) =
  form \props state -> case runUI run props state of
    { render, return } ->
      { render:
          \setState ->
            [ Virtual.Wrapper
                { key: Just key
                , wrap: React.keyed key <<< React.fragment
                , children: render setState
                }
            ]
      , return
      }

validate ::
  forall props validated unvalidated a b.
  CanValidate unvalidated validated =>
  (a -> Either String b) ->
  Form props unvalidated a ->
  Form props (Validated validated) b
validate runValidator (Form ui) =
  form \props state ->
    let
      { render, return } = runUI ui props (fromValidated state)

      result = sequence (runValidator <$> return)
    in
      { render:
          \setState ->
            let
              inner = render (\f -> setState (review modified <<< f <<< fromValidated))
            in
              case result, unsnoc inner of
                Left error, Just { init, last: Virtual.Field field }
                  | isModified state -> init <> [ Virtual.Field field { errors = field.errors <> [ error ] } ]
                Left error, _
                  | isModified state -> inner <> [ Virtual.Error error ]
                _, _ -> inner
      , return:
          case result of
            Right validated -> validated
            _ -> Invalid
      }

derive instance functorForm :: Functor (Form props state)

instance applyForm :: Apply (Form props state) where
  apply (Form f) (Form a) = Form (lift2 apply f a)

instance applicativeForm :: Applicative (Form props state) where
  pure a = Form (pure (pure a))

instance altForm :: Alt (Form props state) where
  alt (Form left) (Form right) =
    form \props state -> case runUI left props state of
      ui@{ return: Valid _ } -> ui
      _ -> runUI right props state

instance plusForm :: Plus (Form props state) where
  empty = form \_ _ -> { render: \_ -> mempty, return: Invalid }

instance alternativeForm :: Alternative (Form props state)

instance focusStateForm :: FocusState (Form props) where
  focus optic (Form ui) = Form (focus optic ui)

derive instance functorSeqForm :: Functor (SeqForm props state)

instance applySeqForm :: Apply (SeqForm props state) where
  apply = ap

instance applicativeSeqForm :: Applicative (SeqForm props state) where
  pure a = SeqForm (pure (pure a))

instance bindSeqForm :: Bind (SeqForm props state) where
  bind (SeqForm run) f =
    SeqForm
      $ ui \props state -> case runUI run props state of
          { render, return: Valid a } -> case f a of
            SeqForm ui' -> case runUI ui' props state of
              { render: render', return } ->
                { render: \setState -> render setState <> render' setState
                , return
                }
          { render, return: Invalid } -> { render, return: Invalid }

instance monadSeqForm :: Monad (SeqForm props state)

instance altSeqForm :: Alt (SeqForm props state) where
  alt (SeqForm left) (SeqForm right) =
    SeqForm
      $ ui \props state -> case runUI left props state of
          ui@{ return: Valid _ } -> ui
          _ -> runUI right props state

instance plusSeqForm :: Plus (SeqForm props state) where
  empty = SeqForm $ ui \_ _ -> { render: \_ -> mempty, return: Invalid }

instance alternativeSeqForm :: Alternative (SeqForm props state)

instance focusStateSeqForm :: FocusState (SeqForm props) where
  focus optic (SeqForm ui) = SeqForm (focus optic ui)
