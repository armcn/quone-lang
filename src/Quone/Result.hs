{-|
Module: Quone.Result

A 'Result' is a value that can succeed with an 'Ok' or fail with an 'Err'.
It is the same idea as Haskell's 'Either', but with the success case on
the right side of the constructor and Elm-style names.

> compile : String -> Result String CompileResult
> compile source =
>     tokenize source
>         |> Result.andThen parse
>         |> Result.andThen typeCheck
-}
module Quone.Result
    ( Result(..)
    , map
    , mapError
    , andThen
    , withDefault
    , fromOk
    , isOk
    , isErr
    , toEither
    , fromEither
    ) where

import Prelude hiding (map)


-- | The result of a computation that may fail.
-- The error is on the left, the success value is on the right.
data Result error value
    = Err error
    | Ok value
    deriving (Show, Eq)


-- The instances below let us use 'do' notation with 'Result'. They behave
-- exactly like the corresponding instances for Haskell's 'Either', so 'do'
-- blocks short-circuit on the first 'Err'.
instance Functor (Result e) where
    fmap = map


instance Applicative (Result e) where
    pure = Ok

    rf <*> rx =
        case rf of
            Err e ->
                Err e

            Ok f ->
                map f rx


instance Monad (Result e) where
    return = pure

    rx >>= f =
        case rx of
            Err e ->
                Err e

            Ok x ->
                f x


-- | Apply a function to the success value, leaving the error untouched.
map :: (a -> b) -> Result e a -> Result e b
map f result =
    case result of
        Ok value ->
            Ok (f value)

        Err err ->
            Err err


-- | Apply a function to the error value, leaving the success value untouched.
mapError :: (e -> f) -> Result e a -> Result f a
mapError f result =
    case result of
        Ok value ->
            Ok value

        Err err ->
            Err (f err)


-- | Chain together a sequence of computations that may fail.
-- Equivalent to monadic bind, but spelled like Elm.
andThen :: (a -> Result e b) -> Result e a -> Result e b
andThen f result =
    case result of
        Ok value ->
            f value

        Err err ->
            Err err


-- | Get the success value or fall back to a default.
withDefault :: a -> Result e a -> a
withDefault def result =
    case result of
        Ok value ->
            value

        Err _ ->
            def


-- | Get the success value, or crash with the error if there isn't one.
-- Use this only when you know the result is 'Ok'.
fromOk :: Show e => Result e a -> a
fromOk result =
    case result of
        Ok value ->
            value

        Err err ->
            error ("Result.fromOk on Err: " ++ show err)


isOk :: Result e a -> Bool
isOk result =
    case result of
        Ok _ ->
            True

        Err _ ->
            False


isErr :: Result e a -> Bool
isErr result =
    case result of
        Ok _ ->
            False

        Err _ ->
            True


-- | Convert to a Haskell 'Either' for interop with libraries that use it.
toEither :: Result e a -> Either e a
toEither result =
    case result of
        Ok x ->
            Right x

        Err e ->
            Left e


-- | Convert from a Haskell 'Either'. Useful at the boundary with library code.
fromEither :: Either e a -> Result e a
fromEither e =
    case e of
        Right x ->
            Ok x

        Left err ->
            Err err
