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

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data Ty
  = TyVar Int
  | TyInt
  | TyDouble
  | TyBool
  | TyStr
  | TyFunc [Ty] Ty
  | TyAdt String [Ty]
  | TyRecord [(String, Ty)]
  deriving (Show, Eq)

data Scheme = Scheme
  { schemeVars :: [Int]
  , schemeTy   :: Ty
  } deriving (Show)

collectFree :: Ty -> Set.Set Int
collectFree ty = case ty of
  TyVar i       -> Set.singleton i
  TyInt         -> Set.empty
  TyDouble      -> Set.empty
  TyBool        -> Set.empty
  TyStr         -> Set.empty
  TyFunc ps r   -> Set.unions (map collectFree ps) `Set.union` collectFree r
  TyAdt _ args  -> Set.unions (map collectFree args)
  TyRecord fs   -> Set.unions (map (collectFree . snd) fs)

substScheme :: Ty -> Map.Map Int Ty -> Ty
substScheme ty m = case ty of
  TyVar i      -> Map.findWithDefault (TyVar i) i m
  TyInt        -> TyInt
  TyDouble     -> TyDouble
  TyBool       -> TyBool
  TyStr        -> TyStr
  TyFunc ps r  -> TyFunc (map (\p -> substScheme p m) ps) (substScheme r m)
  TyAdt n args -> TyAdt n (map (\a -> substScheme a m) args)
  TyRecord fs  -> TyRecord (map (\(n, t) -> (n, substScheme t m)) fs)

assignVarNames :: Ty -> Map.Map Int String -> Int -> (Map.Map Int String, Int)
assignVarNames ty names counter = case ty of
  TyVar i ->
    if Map.member i names
    then (names, counter)
    else (Map.insert i ("t" ++ show counter) names, counter + 1)
  TyInt    -> (names, counter)
  TyDouble -> (names, counter)
  TyBool   -> (names, counter)
  TyStr    -> (names, counter)
  TyFunc ps r ->
    let (names', counter') = foldl (\(n, c) p -> assignVarNames p n c) (names, counter) ps
    in assignVarNames r names' counter'
  TyAdt _ args ->
    foldl (\(n, c) a -> assignVarNames a n c) (names, counter) args
  TyRecord fs ->
    foldl (\(n, c) (_, t) -> assignVarNames t n c) (names, counter) fs

needsParensInApp :: Ty -> Bool
needsParensInApp (TyFunc _ _)   = True
needsParensInApp (TyAdt _ args) = not (null args)
needsParensInApp _              = False

formatTy :: Ty -> String
formatTy ty =
  let (names, _) = assignVarNames ty Map.empty 0
  in formatTyWith names [] ty

formatTyWith :: Map.Map Int String -> [(String, [(String, Ty)])] -> Ty -> String
formatTyWith names aliases ty = case ty of
  TyVar i -> Map.findWithDefault ("?" ++ show i) i names
  TyInt    -> "Integer"
  TyDouble -> "Double"
  TyBool   -> "Logical"
  TyStr    -> "Character"
  TyFunc ps r ->
    let parts = map fmtParam ps ++ [formatTyWith names aliases r]
    in unwords (intersperse' "->" parts)
  TyAdt n [] -> n
  TyAdt n args ->
    let as = map (\a ->
                    let s = formatTyWith names aliases a
                    in if needsParensInApp a then "(" ++ s ++ ")" else s
                 ) args
    in n ++ " " ++ unwords as
  TyRecord fields ->
    case findAlias fields aliases of
      Just aliasName -> aliasName
      Nothing ->
        let fs = map (\(n, t) -> n ++ " : " ++ formatTyWith names aliases t) fields
        in "{ " ++ intercalate' ", " fs ++ " }"
  where
    fmtParam p =
      let s = formatTyWith names aliases p
      in case p of
           TyFunc _ _ -> "(" ++ s ++ ")"
           _          -> s

findAlias :: [(String, Ty)] -> [(String, [(String, Ty)])] -> Maybe String
findAlias fields aliases = go aliases
  where
    go [] = Nothing
    go ((aliasName, aliasFields) : rest)
      | length fields == length aliasFields
        && all matchField (zip fields aliasFields) = Just aliasName
      | otherwise = go rest
    matchField ((n1, _), (n2, _)) = n1 == n2

intersperse' :: a -> [a] -> [a]
intersperse' _ []     = []
intersperse' _ [x]    = [x]
intersperse' sep (x:xs) = x : sep : intersperse' sep xs

intercalate' :: String -> [String] -> String
intercalate' sep = concat . intersperse' sep
