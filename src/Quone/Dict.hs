{-|
Module: Quone.Dict

A dictionary mapping keys to values, modelled after Elm's @Dict@.
Wraps "Data.Map.Strict" with Elm-style names.

> import qualified Quone.Dict as Dict
-}
module Quone.Dict
    ( Dict
    , empty
    , singleton
    , insert
    , remove
    , member
    , get
    , size
    , isEmpty
    , keys
    , values
    , toList
    , fromList
    , map
    , filter
    , foldl
    , union
    ) where

import Prelude hiding (map, filter, foldl, lookup)
import qualified Data.Map.Strict as Map


-- | A dictionary keyed by @comparable@ (Haskell calls this 'Ord').
type Dict k v = Map.Map k v


empty :: Dict k v
empty =
    Map.empty


singleton :: k -> v -> Dict k v
singleton =
    Map.singleton


-- | Insert a key/value pair. Replaces an existing value at the same key.
insert :: Ord k => k -> v -> Dict k v -> Dict k v
insert =
    Map.insert


remove :: Ord k => k -> Dict k v -> Dict k v
remove =
    Map.delete


member :: Ord k => k -> Dict k v -> Bool
member =
    Map.member


-- | Look up a key. Returns 'Nothing' if the key isn't there.
get :: Ord k => k -> Dict k v -> Maybe v
get =
    Map.lookup


size :: Dict k v -> Int
size =
    Map.size


isEmpty :: Dict k v -> Bool
isEmpty =
    Map.null


keys :: Dict k v -> [k]
keys =
    Map.keys


values :: Dict k v -> [v]
values =
    Map.elems


toList :: Dict k v -> [( k, v )]
toList =
    Map.toList


fromList :: Ord k => [( k, v )] -> Dict k v
fromList =
    Map.fromList


-- | Apply a function to each value.
map :: (v -> w) -> Dict k v -> Dict k w
map =
    Map.map


-- | Keep only entries whose value satisfies the predicate.
filter :: (v -> Bool) -> Dict k v -> Dict k v
filter =
    Map.filter


-- | Fold over entries with the value as the first argument.
foldl :: (v -> b -> b) -> b -> Dict k v -> b
foldl f z d =
    Map.foldr f z d


-- | Combine two dictionaries. On collision, prefer the value from the left.
union :: Ord k => Dict k v -> Dict k v -> Dict k v
union =
    Map.union
