{-|
Module: Quone.String

Elm-style operations on strings. We treat 'String' as the same type Haskell
already calls 'String' (a list of characters), so most functions are
re-exports.
-}
module Quone.String
    ( length
    , isEmpty
    , reverse
    , append
    , concat
    , join
    , split
    , words
    , lines
    , trim
    , trimLeft
    , trimRight
    , toLower
    , toUpper
    , startsWith
    , endsWith
    , contains
    , replace
    , dropLeft
    , dropRight
    , left
    , right
    , fromInt
    , fromFloat
    , toInt
    , toFloat
    , toList
    , fromList
    ) where

import Prelude hiding
    ( length, reverse, concat, words, lines, words, lines )
import qualified Data.Char as Char
import qualified Data.List as L


length :: String -> Int
length =
    L.length


isEmpty :: String -> Bool
isEmpty s =
    case s of
        "" ->
            True

        _ ->
            False


reverse :: String -> String
reverse =
    L.reverse


append :: String -> String -> String
append =
    (++)


concat :: [String] -> String
concat =
    L.concat


-- | Join strings using a separator.
join :: String -> [String] -> String
join =
    L.intercalate


-- | Split a string at every occurrence of a separator character.
split :: Char -> String -> [String]
split sep =
    go []
  where
    go acc "" =
        [reverse acc]

    go acc (c : rest)
        | c == sep =
            reverse acc : go [] rest

        | otherwise =
            go (c : acc) rest


words :: String -> [String]
words =
    L.words


lines :: String -> [String]
lines =
    L.lines


trim :: String -> String
trim =
    trimLeft << trimRight


trimLeft :: String -> String
trimLeft =
    dropWhile Char.isSpace


trimRight :: String -> String
trimRight =
    L.reverse << dropWhile Char.isSpace << L.reverse


toLower :: String -> String
toLower =
    L.map Char.toLower


toUpper :: String -> String
toUpper =
    L.map Char.toUpper


startsWith :: String -> String -> Bool
startsWith =
    L.isPrefixOf


endsWith :: String -> String -> Bool
endsWith =
    L.isSuffixOf


contains :: String -> String -> Bool
contains =
    L.isInfixOf


-- | Replace every occurrence of one string with another.
replace :: String -> String -> String -> String
replace from to =
    go
  where
    fromLen =
        L.length from

    go [] =
        []

    go s@(c : rest)
        | from `L.isPrefixOf` s =
            to ++ go (L.drop fromLen s)

        | otherwise =
            c : go rest


dropLeft :: Int -> String -> String
dropLeft n s
    | n <= 0 =
        s

    | otherwise =
        L.drop n s


dropRight :: Int -> String -> String
dropRight n s
    | n <= 0 =
        s

    | otherwise =
        L.take (L.length s - n) s


left :: Int -> String -> String
left =
    L.take


right :: Int -> String -> String
right n s =
    L.drop (L.length s - n) s


fromInt :: Int -> String
fromInt =
    show


fromFloat :: Double -> String
fromFloat =
    show


toInt :: String -> Maybe Int
toInt s =
    case reads s of
        [( n, "" )] ->
            Just n

        _ ->
            Nothing


toFloat :: String -> Maybe Double
toFloat s =
    case reads s of
        [( f, "" )] ->
            Just f

        _ ->
            Nothing


toList :: String -> [Char]
toList =
    id


fromList :: [Char] -> String
fromList =
    id


-- | Compose right-to-left, mimicking 'Quone.Prelude.<<'.
(<<) :: (b -> c) -> (a -> b) -> a -> c
g << f =
    \x -> g (f x)
infixr 9 <<
