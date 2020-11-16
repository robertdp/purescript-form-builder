module Data.UI.Form.Virtual where

import Prelude
import Data.Array (mapMaybe)
import Data.Maybe (Maybe(..))
import React.Basic (JSX)

data Form
  = Error String
  | Child
    { key :: Maybe String
    , child :: JSX
    }
  | Wrapper
    { key :: Maybe String
    , wrap :: Array JSX -> JSX
    , children :: Array Form
    }
  | Field
    { key :: Maybe String
    , label :: Maybe JSX
    , required :: Maybe FieldRequirement
    , errors :: Array String
    , warnings :: Array String
    , children :: Array Form
    }

data FieldRequirement
  = Required
  | Optional

type Tree
  = Array Form

prune :: Tree -> Tree
prune =
  mapMaybe case _ of
    e@(Error _) -> Just e
    c@(Child _) -> Just c
    Wrapper w@{ children } -> case prune children of
      [] -> Nothing
      children' -> Just (Wrapper w { children = children' })
    Field f@{ children } -> case prune children of
      [] -> Nothing
      children' -> Just (Field f { children = children' })

render :: forall a. (Tree -> a) -> Tree -> a
render f = f <<< prune
