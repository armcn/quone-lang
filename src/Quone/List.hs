{-|
Module: Quone.List

An Elm-style API for lists. Use it qualified:

> import qualified Quone.List as List

Most functions are thin wrappers around the standard Haskell list functions
with Elm-style argument orders (the list usually comes last).
-}
module Quone.List
    ( map
    , filter
    , foldl
    , foldr
    , concat
    , concatMap
    , reverse
    , length
    , isEmpty
    , head
    , tail
    , drop
    , take
    , append
    , singleton
    , range
    , member
    , all
    , any
    , find
    , sortBy
    , sortOn
    , zip
    , zipWith
    , unzip
    , partition
    , indexedMap
    , intersperse
    , intercalate
    , unique
    ) where

import Prelude hiding
    ( map, filter, foldl, foldr, concat, concatMap, reverse, length
    , head, tail, drop, take, all, any, zip, zipWith, unzip
    )
import qualified Data.List as L
import qualified Data.Set as Set


map :: (a -> b) -> List a -> List b
map =
    L.map


filter :: (a -> Bool) -> List a -> List a
filter =
    L.filter


foldl :: (a -> b -> b) -> b -> List a -> b
foldl f =
    L.foldl' (flip f)


foldr :: (a -> b -> b) -> b -> List a -> b
foldr =
    L.foldr


concat :: List (List a) -> List a
concat =
    L.concat


concatMap :: (a -> List b) -> List a -> List b
concatMap =
    L.concatMap


reverse :: List a -> List a
reverse =
    L.reverse


length :: List a -> Int
length =
    L.length


isEmpty :: List a -> Bool
isEmpty xs =
    case xs of
        [] ->
            True

        _ ->
            False


-- | The first element, or 'Nothing' if empty.
head :: List a -> Maybe a
head xs =
    case xs of
        [] ->
            Nothing

        x : _ ->
            Just x


-- | Everything except the first element, or 'Nothing' if empty.
tail :: List a -> Maybe (List a)
tail xs =
    case xs of
        [] ->
            Nothing

        _ : rest ->
            Just rest


drop :: Int -> List a -> List a
drop =
    L.drop


take :: Int -> List a -> List a
take =
    L.take


append :: List a -> List a -> List a
append =
    (++)


singleton :: a -> List a
singleton x =
    [x]


-- | A list of integers from low to high (inclusive).
range :: Int -> Int -> List Int
range low high =
    [ low .. high ]


member :: Eq a => a -> List a -> Bool
member x xs =
    L.elem x xs


all :: (a -> Bool) -> List a -> Bool
all =
    L.all


any :: (a -> Bool) -> List a -> Bool
any =
    L.any


find :: (a -> Bool) -> List a -> Maybe a
find =
    L.find


-- | Sort using a comparison function.
sortBy :: (a -> a -> Ordering) -> List a -> List a
sortBy =
    L.sortBy


-- | Sort by projecting each element to a comparable value.
sortOn :: Ord b => (a -> b) -> List a -> List a
sortOn =
    L.sortOn


zip :: List a -> List b -> List ( a, b )
zip =
    L.zip


zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith =
    L.zipWith


unzip :: List ( a, b ) -> ( List a, List b )
unzip =
    L.unzip


partition :: (a -> Bool) -> List a -> ( List a, List a )
partition =
    L.partition


-- | Map with the index passed in as the first argument.
indexedMap :: (Int -> a -> b) -> List a -> List b
indexedMap f xs =
    L.zipWith f [0 ..] xs


intersperse :: a -> List a -> List a
intersperse =
    L.intersperse


-- | Join a list of strings with a separator.
intercalate :: List a -> List (List a) -> List a
intercalate =
    L.intercalate


-- | Remove duplicates, keeping the first occurrence.
unique :: Ord a => List a -> List a
unique =
    go Set.empty
  where
    go _ [] =
        []

    go seen (x : xs)
        | Set.member x seen =
            go seen xs

        | otherwise =
            x : go (Set.insert x seen) xs


-- The List alias from Quone.Prelude. We don't import it here because of
-- the cyclic dependency, so we redefine it locally.
type List a = [a]
