{-|
Module: Quone.Maybe

An Elm-style API for 'Maybe'. The 'Maybe' type itself is the same as
Haskell's, with the same constructors ('Nothing' and 'Just'), so we don't
re-export it.

> import qualified Quone.Maybe as Maybe
-}
module Quone.Maybe
    ( map
    , withDefault
    , andThen
    , isJust
    , isNothing
    , fromMaybe
    ) where

import Prelude hiding (map)
import qualified Data.Maybe as M


map :: (a -> b) -> Maybe a -> Maybe b
map =
    fmap


-- | Get the value out of a 'Maybe', or fall back to a default.
withDefault :: a -> Maybe a -> a
withDefault def m =
    case m of
        Just x ->
            x

        Nothing ->
            def


-- | Chain together computations that may produce 'Nothing'.
andThen :: (a -> Maybe b) -> Maybe a -> Maybe b
andThen f m =
    case m of
        Just x ->
            f x

        Nothing ->
            Nothing


isJust :: Maybe a -> Bool
isJust =
    M.isJust


isNothing :: Maybe a -> Bool
isNothing =
    M.isNothing


-- | Same as 'withDefault' but with arguments flipped to match Data.Maybe.
fromMaybe :: a -> Maybe a -> a
fromMaybe =
    M.fromMaybe
