{-|
Module: Quone.Set

An unordered collection of unique values, modelled after Elm's @Set@.
Wraps "Data.Set" with Elm-style names.
-}
module Quone.Set
    ( Set
    , empty
    , singleton
    , insert
    , remove
    , member
    , size
    , isEmpty
    , toList
    , fromList
    , union
    , intersect
    , diff
    ) where

import qualified Data.Set as S


type Set a = S.Set a


empty :: Set a
empty =
    S.empty


singleton :: a -> Set a
singleton =
    S.singleton


insert :: Ord a => a -> Set a -> Set a
insert =
    S.insert


remove :: Ord a => a -> Set a -> Set a
remove =
    S.delete


member :: Ord a => a -> Set a -> Bool
member =
    S.member


size :: Set a -> Int
size =
    S.size


isEmpty :: Set a -> Bool
isEmpty =
    S.null


toList :: Set a -> [a]
toList =
    S.toList


fromList :: Ord a => [a] -> Set a
fromList =
    S.fromList


union :: Ord a => Set a -> Set a -> Set a
union =
    S.union


intersect :: Ord a => Set a -> Set a -> Set a
intersect =
    S.intersection


diff :: Ord a => Set a -> Set a -> Set a
diff =
    S.difference
