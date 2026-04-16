{-|
Module: Quone.Type.Type

The internal representation of types used during inference and code
generation.

  - 'Ty' is a single type — like @Vector Double@ or @Integer -> Integer@.
  - 'Scheme' is a polymorphic type with quantified variables — like
    @forall a. List a -> Int@. We use schemes when storing functions in
    the environment so that each call site gets fresh type variables.

The two functions you'll most often want from this module are:

  - 'formatTy' — turn a 'Ty' into a human-readable string for error messages
  - 'collectFree' — find every free type variable in a 'Ty', used by
    generalization
-}
module Quone.Type.Type
    ( Ty(..)
    , Scheme(..)
    , formatTy
    , formatTyWith
    , collectFree
    , substScheme
    , assignVarNames
    , needsParensInApp
    ) where

import Quone.Prelude
import qualified Quone.Dict as Dict
import qualified Quone.List as List
import qualified Quone.Set as Set
import Quone.Set (Set)
import Quone.Dict (Dict)


-- | An internal type. Every Quone expression has one of these.
data Ty
    = TyVar Int
      -- ^ A type variable. The 'Int' is just an identifier.

    | TyInt
    | TyDouble
    | TyBool
    | TyStr
    | TyFunc (List Ty) Ty
      -- ^ A function: parameter types and return type.

    | TyAdt String (List Ty)
      -- ^ A user-defined type applied to its arguments. @TyAdt "Vector" [TyInt]@
      -- represents @Vector Integer@.

    | TyRecord (List ( String, Ty ))
      -- ^ A record (or dataframe): a list of named fields.
    deriving (Show, Eq)


-- | A polymorphic type. The 'List Int' lists which type variables are
-- generalized; 'Ty' is the body.
data Scheme = Scheme
    { schemeVars :: List Int
    , schemeTy :: Ty
    }
    deriving (Show)


-- | Every type variable that appears in a type.
collectFree :: Ty -> Set Int
collectFree ty =
    case ty of
        TyVar i ->
            Set.singleton i

        TyInt ->
            Set.empty

        TyDouble ->
            Set.empty

        TyBool ->
            Set.empty

        TyStr ->
            Set.empty

        TyFunc ps r ->
            List.foldl Set.union (collectFree r) (List.map collectFree ps)

        TyAdt _ args ->
            List.foldl Set.union Set.empty (List.map collectFree args)

        TyRecord fs ->
            List.foldl Set.union Set.empty (List.map (collectFree << snd) fs)


-- | Walk a type, replacing every type variable that appears as a key in
-- the substitution with its mapped value.
substScheme :: Ty -> Dict Int Ty -> Ty
substScheme ty subs =
    case ty of
        TyVar i ->
            case Dict.get i subs of
                Just t ->
                    t

                Nothing ->
                    TyVar i

        TyInt ->
            TyInt

        TyDouble ->
            TyDouble

        TyBool ->
            TyBool

        TyStr ->
            TyStr

        TyFunc ps r ->
            TyFunc (List.map (\p -> substScheme p subs) ps) (substScheme r subs)

        TyAdt n args ->
            TyAdt n (List.map (\a -> substScheme a subs) args)

        TyRecord fs ->
            TyRecord (List.map (\( n, t ) -> ( n, substScheme t subs )) fs)


-- | Walk a type and assign a friendly name (@a@, @b@, ...) to each
-- distinct type variable. Returns the updated dictionary and the next
-- counter value, so you can chain calls together.
assignVarNames :: Ty -> Dict Int String -> Int -> ( Dict Int String, Int )
assignVarNames ty names counter =
    case ty of
        TyVar i ->
            if Dict.member i names then
                ( names, counter )
            else
                ( Dict.insert i ("t" ++ show counter) names, counter + 1 )

        TyInt ->
            ( names, counter )

        TyDouble ->
            ( names, counter )

        TyBool ->
            ( names, counter )

        TyStr ->
            ( names, counter )

        TyFunc ps r ->
            let
                step ( n, c ) p =
                    assignVarNames p n c

                ( names1, counter1 ) =
                    List.foldl (flip step) ( names, counter ) ps
            in
            assignVarNames r names1 counter1

        TyAdt _ args ->
            let
                step ( n, c ) a =
                    assignVarNames a n c
            in
            List.foldl (flip step) ( names, counter ) args

        TyRecord fs ->
            let
                step ( n, c ) ( _, t ) =
                    assignVarNames t n c
            in
            List.foldl (flip step) ( names, counter ) fs


-- | Whether a type needs parentheses when it appears as the argument of
-- another type. @Vector a@ doesn't, but @(Vector a)@ does inside @Maybe@.
needsParensInApp :: Ty -> Bool
needsParensInApp ty =
    case ty of
        TyFunc _ _ ->
            True

        TyAdt _ args ->
            not (List.isEmpty args)

        _ ->
            False


-- | Pretty-print a type for use in error messages.
formatTy :: Ty -> String
formatTy ty =
    let
        ( names, _ ) =
            assignVarNames ty Dict.empty 0
    in
    formatTyWith names [] ty


-- | Pretty-print with a pre-existing variable-name dictionary and a list
-- of known type aliases. The aliases are used to print @Students@ instead
-- of @{ name : Vector Character, score : Vector Double, ... }@ when the
-- shape matches.
formatTyWith
    :: Dict Int String
    -> List ( String, List ( String, Ty ) )
    -> Ty
    -> String
formatTyWith names aliases ty =
    case ty of
        TyVar i ->
            case Dict.get i names of
                Just name ->
                    name

                Nothing ->
                    "?" ++ show i

        TyInt ->
            "Integer"

        TyDouble ->
            "Double"

        TyBool ->
            "Logical"

        TyStr ->
            "Character"

        TyFunc ps r ->
            let
                fmtParam p =
                    case p of
                        TyFunc _ _ ->
                            "(" ++ formatTyWith names aliases p ++ ")"

                        _ ->
                            formatTyWith names aliases p

                parts =
                    List.map fmtParam ps ++ [ formatTyWith names aliases r ]
            in
            joinWith " -> " parts

        TyAdt n [] ->
            n

        TyAdt n args ->
            let
                fmtArg a =
                    let
                        s =
                            formatTyWith names aliases a
                    in
                    if needsParensInApp a then
                        "(" ++ s ++ ")"
                    else
                        s
            in
            n ++ " " ++ joinWith " " (List.map fmtArg args)

        TyRecord fields ->
            case findAlias fields aliases of
                Just name ->
                    name

                Nothing ->
                    let
                        fs =
                            List.map
                                (\( n, t ) -> n ++ " : " ++ formatTyWith names aliases t)
                                fields
                    in
                    "{ " ++ joinWith ", " fs ++ " }"


-- | Look up a record shape in the alias table. We match purely on field
-- names (in the same order); the field types could differ at this point.
findAlias :: List ( String, Ty ) -> List ( String, List ( String, Ty ) ) -> Maybe String
findAlias fields aliases =
    case aliases of
        [] ->
            Nothing

        ( aliasName, aliasFields ) : rest ->
            if sameShape fields aliasFields then
                Just aliasName
            else
                findAlias fields rest


sameShape :: List ( String, Ty ) -> List ( String, Ty ) -> Bool
sameShape xs ys =
    List.length xs == List.length ys
        && List.all (\( ( n1, _ ), ( n2, _ ) ) -> n1 == n2) (List.zip xs ys)


-- | Join a list of strings with a separator. We use this instead of
-- 'List.intercalate' here just to make calls read more like Elm.
joinWith :: String -> List String -> String
joinWith sep parts =
    List.intercalate sep parts
