{-|
Module: Quone.Prelude

A small Elm-style prelude used throughout the Quone compiler.

The goal is to make this Haskell code feel like Elm code:
  - 'Result' instead of 'Either'
  - 'List' as an alias for [a]
  - '|>' for forward function application (the pipe operator)
  - '<<' and '>>' for function composition
  - 'identity', 'always' as plain functions

We re-export only what we want available everywhere. Modules import
specific helpers from 'Quone.List', 'Quone.Maybe', 'Quone.Result' etc.
qualified, just as you would in Elm:

> import qualified Quone.List as List
> import qualified Quone.Maybe as Maybe
-}
module Quone.Prelude
    ( -- The Result type re-exported
      Result(..)

      -- The List type alias
    , List

      -- The pipe and composition operators
    , (|>)
    , (<|)
    , (<<)
    , (>>)

      -- Tiny helpers
    , identity
    , always

      -- Re-exports from the standard Prelude
    , module Prelude
    ) where

import Prelude hiding ((>>))

import Quone.Result (Result(..))


-- | A list. Same as Haskell's [a], but spelled the Elm way.
type List a = [a]


-- | Forward function application. Read @x |> f@ as "take x and pass it to f".
infixl 1 |>
(|>) :: a -> (a -> b) -> b
x |> f =
    f x


-- | Backward function application. Read @f <| x@ as "apply f to x".
infixr 0 <|
(<|) :: (a -> b) -> a -> b
f <| x =
    f x


-- | Function composition that reads left to right.
-- @(f >> g) x@ is the same as @g (f x)@.
infixl 9 >>
(>>) :: (a -> b) -> (b -> c) -> a -> c
f >> g =
    \x -> g (f x)


-- | Function composition that reads right to left.
-- @(g << f) x@ is the same as @g (f x)@.
infixr 9 <<
(<<) :: (b -> c) -> (a -> b) -> a -> c
g << f =
    \x -> g (f x)


-- | The identity function.
identity :: a -> a
identity x =
    x


-- | Always return the same value, ignoring whatever was passed in.
-- Useful in higher-order code: @List.map (always 0) xs@.
always :: a -> b -> a
always x _ =
    x
