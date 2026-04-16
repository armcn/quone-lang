module Quone
  ( compile
  , compileToR
  , check
  , hoverInfo
  , formatSource
  , generateNamespace
  , generateDescription
  , replCompile
  , CompileResult(..)
  , ModuleInfo(..)
  , CompileError(..)
  , HoverEntry(..)
  , ReplState(..)
  , newReplState
  ) where

import Data.List (intercalate, isPrefixOf, isInfixOf, findIndex, find)
import qualified Data.Map.Strict as Map

import Quone.AST.Source
import Quone.Parse.Lexer (tokenize)
import Quone.Parse.Parser (parseProgram)
import Quone.Type.Type (Ty(..), Scheme(..), formatTyWith, assignVarNames)
import Quone.Type.Infer (InferState(..), inferProgram, runInfer)
import Quone.Generate.R (generate)
import qualified Quone.Format as Fmt

data CompileResult = CompileResult
  { crRCode  :: String
  , crModule :: Maybe ModuleInfo
  }

data ModuleInfo = ModuleInfo
  { miName    :: String
  , miExports :: [String]
  }

data CompileError = CompileError
  { ceMessage :: String
  , ceLine    :: Int
  , ceCol     :: Int
  } deriving (Show)

prelude :: String
prelude = unlines
  [ "import sqrt : Double -> Double"
  , "import mean : Vector a -> Double"
  , "import sum : Vector a -> Double"
  , "import length : Vector a -> Integer"
  , "import to_double : Integer -> Double"
  ]

injectPrelude :: String -> String
injectPrelude source
  | "package " `isPrefixOf` stripped =
    case findIndex (== '\n') source of
      Just pos -> take (pos + 1) source ++ prelude ++ drop (pos + 1) source
      Nothing  -> source ++ "\n" ++ prelude
  | otherwise = prelude ++ source
  where stripped = dropWhile (== ' ') source

compile :: String -> Either String CompileResult
compile source = do
  let fullSource = injectPrelude source
  (toks, sps) <- tokenize fullSource
  prog <- parseProgram toks sps
  let s0 = InferState 0 mempty [] mempty
  (types, inferSt) <- inferProgram s0 prog
  let rCode = generate prog types inferSt
      modInfo = fmap toModuleInfo (progModule prog)
  Right $ CompileResult rCode modInfo
  where
    toModuleInfo m = ModuleInfo
      { miName = moduleName m
      , miExports = resolveExports m
      }
    resolveExports m = case moduleExports m of
      ExportAll -> error "ExportAll requires program context"
      ExportList ns -> ns

compileToR :: String -> Either String String
compileToR source = crRCode <$> compile source

check :: String -> [CompileError]
check source =
  let isPackage = "package " `isPrefixOf` dropWhile (== ' ') source
      preludeLineCount = length (lines prelude)
      lineOffset = if isPackage then 0 else preludeLineCount
      fullSource = injectPrelude source
  in case pipeline fullSource of
    Right _  -> []
    Left msg -> [parseErrorWithFallback msg source lineOffset]
  where
    pipeline src = do
      (toks, sps) <- tokenize src
      prog <- parseProgram toks sps
      let s0 = InferState 0 mempty [] mempty
      _ <- inferProgram s0 prog
      Right ()

parseErrorWithFallback :: String -> String -> Int -> CompileError
parseErrorWithFallback msg source lineOffset =
  case parseLocatedError msg of
    Just (line, col, message) ->
      let adjusted = if line > lineOffset then line - lineOffset else 1
      in CompileError message adjusted col
    Nothing ->
      case findErrorLocation msg source of
        Just (line, col) -> CompileError msg line col
        Nothing -> CompileError msg 1 1

parseLocatedError :: String -> Maybe (Int, Int, String)
parseLocatedError msg =
  case break (== ':') msg of
    (lineStr, ':':rest) ->
      case break (== ':') rest of
        (colStr, ':':message) ->
          case (reads lineStr, reads colStr) of
            ([(line, "")], [(col, "")]) -> Just (line, col, message)
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing

findErrorLocation :: String -> String -> Maybe (Int, Int)
findErrorLocation msg source =
  let srcLines = zip [1..] (lines source)
      innerMsg = stripDefPrefix msg
      specificPatterns =
        [ ("Unknown column '", "'")
        , ("Unknown function or column '", "'")
        , ("Unbound variable: ", "")
        , ("Record has no field '", "'")
        , ("Cannot access field '", "'")
        ]
  in case tryPatterns specificPatterns innerMsg srcLines of
    Just loc -> Just loc
    Nothing -> case tryPatterns specificPatterns msg srcLines of
      Just loc -> Just loc
      Nothing -> case extractName "In definition of '" "'" msg of
        Just name -> findDefBodyLocation name innerMsg srcLines
        Nothing -> case tryKeywords msg srcLines of
          Just loc -> Just loc
          Nothing  -> Nothing

tryPatterns :: [(String, String)] -> String -> [(Int, String)] -> Maybe (Int, Int)
tryPatterns [] _ _ = Nothing
tryPatterns ((prefix, suffix):rest) msg srcLines =
  case extractName prefix suffix msg of
    Just name -> case findNameInLines name srcLines of
      Just loc -> Just loc
      Nothing  -> tryPatterns rest msg srcLines
    Nothing -> tryPatterns rest msg srcLines

tryKeywords :: String -> [(Int, String)] -> Maybe (Int, Int)
tryKeywords msg srcLines =
  let keywords = ["mutate", "filter", "select", "summarize", "arrange", "group_by"]
  in go keywords
  where
    go [] = Nothing
    go (kw:kws)
      | kw `isInfixOf` msg = findNameInLines kw srcLines
      | otherwise = go kws

stripDefPrefix :: String -> String
stripDefPrefix msg =
  let prefix = "In definition of '"
  in case findSubstring prefix msg of
    Nothing -> msg
    Just idx ->
      let afterPrefix = drop (idx + length prefix) msg
          afterName = drop 1 (dropWhile (/= '\'') afterPrefix)
      in case afterName of
        ':':' ':rest -> rest
        ':':rest     -> rest
        _            -> msg

extractName :: String -> String -> String -> Maybe String
extractName prefix suffix msg =
  case findSubstring prefix msg of
    Nothing -> Nothing
    Just idx ->
      let rest = drop (idx + length prefix) msg
          name = if null suffix
                 then takeWhile (\c -> c /= ' ' && c /= '\n') rest
                 else takeWhile (/= head suffix) rest
      in if null name then Nothing else Just name

findSubstring :: String -> String -> Maybe Int
findSubstring _ [] = Nothing
findSubstring needle haystack = go 0 haystack
  where
    go _ [] = Nothing
    go i s
      | needle `isPrefixOf` s = Just i
      | otherwise = go (i + 1) (tail s)

findNameInLines :: String -> [(Int, String)] -> Maybe (Int, Int)
findNameInLines _ [] = Nothing
findNameInLines name srcLines =
  case findDefLine name srcLines of
    Just loc -> Just loc
    Nothing  -> findFirstOccurrence name srcLines

findDefLine :: String -> [(Int, String)] -> Maybe (Int, Int)
findDefLine name srcLines =
  case findBindBody srcLines of
    Just loc -> Just loc
    Nothing  -> findWith (startsWithName name " :") srcLines
  where
    findBindBody [] = Nothing
    findBindBody ((lineNum, lineText):rest')
      | startsWithName name " <-" lineText =
        case rest' of
          ((nextLine, _):_) -> Just (nextLine, 1)
          []                -> Just (lineNum, 1)
      | otherwise = findBindBody rest'
    findWith _ [] = Nothing
    findWith p ((lineNum, lineText):rest')
      | p lineText = Just (lineNum, 1)
      | otherwise  = findWith p rest'

findDefBodyLocation :: String -> String -> [(Int, String)] -> Maybe (Int, Int)
findDefBodyLocation name innerMsg srcLines =
  let bodyLines = getBodyLines name srcLines
      keywords = extractMsgKeywords innerMsg
  in case concatMap (\kw -> findInLines kw bodyLines) keywords of
    (loc:_) -> Just loc
    []      -> case bodyLines of
      ((ln, _):_) -> Just (ln, 1)
      []          -> findNameInLines name srcLines

getBodyLines :: String -> [(Int, String)] -> [(Int, String)]
getBodyLines name srcLines = go srcLines
  where
    go [] = []
    go ((ln, txt):rest)
      | startsWithName name " <-" txt = takeBody rest
      | otherwise = go rest
    takeBody [] = []
    takeBody ((ln, txt):rest)
      | not (null (dropWhile (== ' ') txt)) && head (dropWhile (== ' ') txt) /= '#'
      , let indent = length (takeWhile (== ' ') txt)
      , indent > 0 = (ln, txt) : takeBody rest
      | null (dropWhile (== ' ') txt) = takeBody rest
      | otherwise = []

extractMsgKeywords :: String -> [String]
extractMsgKeywords msg = extractQuoted msg

extractQuoted :: String -> [String]
extractQuoted [] = []
extractQuoted ('\'':rest) =
  let word = takeWhile (/= '\'') rest
      remaining = drop (length word + 1) rest
  in if null word then extractQuoted remaining else word : extractQuoted remaining
extractQuoted (_:rest) = extractQuoted rest

findInLines :: String -> [(Int, String)] -> [(Int, Int)]
findInLines _ [] = []
findInLines needle ((ln, txt):rest) =
  case findSubstring needle txt of
    Just col -> (ln, col + 1) : findInLines needle rest
    Nothing  -> findInLines needle rest

startsWithName :: String -> String -> String -> Bool
startsWithName name suffix lineText =
  let stripped = dropWhile (== ' ') lineText
  in (name ++ suffix) `isPrefixOf` stripped

findFirstOccurrence :: String -> [(Int, String)] -> Maybe (Int, Int)
findFirstOccurrence _ [] = Nothing
findFirstOccurrence name ((lineNum, lineText):rest) =
  case findSubstring name lineText of
    Just col -> Just (lineNum, col + 1)
    Nothing  -> findFirstOccurrence name rest

data HoverEntry = HoverEntry
  { heName     :: String
  , heTypeStr  :: String
  , heLine     :: Int
  , heCol      :: Int
  } deriving (Show)

hoverInfo :: String -> [HoverEntry]
hoverInfo source =
  let isPackage = "package " `isPrefixOf` dropWhile (== ' ') source
      preludeLineCount = length (lines prelude)
      lineOffset = if isPackage then 0 else preludeLineCount
      fullSource = injectPrelude source
  in case pipeline fullSource of
    Left _  -> []
    Right (types, inferSt, prog) ->
      let fmtTy ty =
            let (names, _) = assignVarNames ty Map.empty 0
            in formatTyWith names [] ty
          entries = concatMap (mkEntry fmtTy lineOffset source prog inferSt) types
          builtinEntries = mkBuiltinEntries fmtTy inferSt (map fst types)
      in entries ++ builtinEntries
  where
    pipeline src = do
      (toks, sps) <- tokenize src
      prog <- parseProgram toks sps
      let s0 = InferState 0 mempty [] mempty
      (types, inferSt) <- inferProgram s0 prog
      Right (types, inferSt, prog)

mkEntry :: (Ty -> String) -> Int -> String -> Program -> InferState -> (String, Ty) -> [HoverEntry]
mkEntry fmtTy lineOffset source prog inferSt (name, ty) =
  let tyStr = fmtTy ty
      (line, col) = findNameInSource name source
      entry = HoverEntry name tyStr line col
      paramEntries = case findLetDecl name prog of
        Just ld -> mkParamEntries fmtTy source ld ty
        Nothing -> []
  in entry : paramEntries

findLetDecl :: String -> Program -> Maybe LetDecl
findLetDecl name prog = find (\ld -> letName ld == name) [ld | DeclLet ld <- progDecls prog]

mkParamEntries :: (Ty -> String) -> String -> LetDecl -> Ty -> [HoverEntry]
mkParamEntries fmtTy source ld ty = case (letExpr ld, ty) of
  (ELambda params _, TyFunc paramTys _) ->
    [ HoverEntry p (fmtTy pt) line col
    | (p, pt) <- zip params paramTys
    , p /= "_"
    , let (line, col) = findNameInSource p source
    ]
  _ -> []

mkBuiltinEntries :: (Ty -> String) -> InferState -> [String] -> [HoverEntry]
mkBuiltinEntries fmtTy inferSt seen =
  let builtins = ["map", "map2", "reduce", "keep", "discard"]
  in concatMap (\bname ->
    if bname `elem` seen then []
    else case find (\(n, _) -> n == bname) (env inferSt) of
      Just (_, sc) ->
        let ty = instantiateForDisplay inferSt sc
            tyStr = fmtTy ty
        in [HoverEntry bname tyStr 0 0]
      Nothing -> []
    ) builtins

instantiateForDisplay :: InferState -> Scheme -> Ty
instantiateForDisplay s sc =
  let mapping = Map.fromList [(v, TyVar v) | v <- schemeVars sc]
  in applySubst s (substTy (schemeTy sc) mapping)

substTy :: Ty -> Map.Map Int Ty -> Ty
substTy ty m = case ty of
  TyVar i      -> Map.findWithDefault (TyVar i) i m
  TyInt        -> TyInt
  TyDouble     -> TyDouble
  TyBool       -> TyBool
  TyStr        -> TyStr
  TyFunc ps r  -> TyFunc (map (\p -> substTy p m) ps) (substTy r m)
  TyAdt n args -> TyAdt n (map (\a -> substTy a m) args)
  TyRecord fs  -> TyRecord (map (\(n, t) -> (n, substTy t m)) fs)

applySubst :: InferState -> Ty -> Ty
applySubst s ty = case ty of
  TyVar i -> case Map.lookup i (subst s) of
    Just resolved -> applySubst s resolved
    Nothing       -> TyVar i
  TyInt    -> TyInt
  TyDouble -> TyDouble
  TyBool   -> TyBool
  TyStr    -> TyStr
  TyFunc ps r  -> TyFunc (map (applySubst s) ps) (applySubst s r)
  TyAdt n args -> TyAdt n (map (applySubst s) args)
  TyRecord fs  -> TyRecord (map (\(n, t) -> (n, applySubst s t)) fs)

findNameInSource :: String -> String -> (Int, Int)
findNameInSource name source =
  case findNameInLines name (zip [1..] (lines source)) of
    Just (l, c) -> (l, c)
    Nothing     -> (1, 1)

formatSource :: String -> Either String String
formatSource = Fmt.formatSource

generateNamespace :: ModuleInfo -> String
generateNamespace info =
  concatMap (\n -> "export(" ++ n ++ ")\n") (miExports info)

generateDescription :: ModuleInfo -> String
generateDescription info = intercalate "\n"
  [ "Package: " ++ miName info
  , "Title: " ++ miName info
  , "Version: 0.1.0"
  , "Description: Generated by quone."
  , "License: MIT"
  , "Encoding: UTF-8"
  , "Roxygen: list(markdown = TRUE)"
  , "RoxygenNote: 7.3.0"
  , ""
  ]

data ReplState = ReplState
  { rsSource   :: String
  , rsInferSt  :: InferState
  , rsTypes    :: [(String, Ty)]
  }

newReplState :: ReplState
newReplState = ReplState prelude (InferState 0 mempty [] mempty) []

replCompile :: ReplState -> String -> Either String (String, String, ReplState)
replCompile rs input =
  case tryAsDecl of
    Right result -> Right result
    Left _ -> case tryAsExpr of
      Right result -> Right result
      Left e -> Left e
  where
    tryAsDecl =
      let fullSource = rsSource rs ++ "\n" ++ input
      in case pipeline fullSource False of
        Right (rCode, types, inferSt) ->
          let newTypes = drop (length (rsTypes rs)) types
          in Right (rCode, formatNewTypes newTypes, ReplState fullSource inferSt types)
        Left e -> Left e

    tryAsExpr =
      let wrapped = "it <- " ++ input
          fullSource = rsSource rs ++ "\n" ++ wrapped
      in case pipeline fullSource True of
        Right (rCode, _types, inferSt) ->
          let printR = rCode ++ "print(it)\n"
          in Right (printR, "", rsSource rs `seq` ReplState (rsSource rs) inferSt (rsTypes rs))
        Left e -> Left e

    pipeline src printExpr = do
      (toks, sps) <- tokenize src
      prog <- parseProgram toks sps
      let s0 = InferState 0 mempty [] mempty
      (types, inferSt) <- inferProgram s0 prog
      let rCode = generate prog types inferSt
          prevR = case compileSource (rsSource rs) of
                    Right r  -> r
                    Left _   -> ""
          newR = stripPrefix' rCode prevR
      if printExpr
        then Right (newR, types, inferSt)
        else Right (newR, types, inferSt)

    compileSource src = do
      (toks, sps) <- tokenize src
      prog <- parseProgram toks sps
      let s0 = InferState 0 mempty [] mempty
      (types, inferSt) <- inferProgram s0 prog
      Right $ generate prog types inferSt

    stripPrefix' new old =
      let newLines = lines new
          oldLines = lines old
          dropped = drop (length oldLines) newLines
      in unlines dropped

    formatNewTypes [] = ""
    formatNewTypes ts =
      let (names, _) = foldl (\(m, c) (_, ty) -> assignVarNames ty m c) (Map.empty, 0) ts
      in concatMap (\(n, ty) -> n ++ " : " ++ formatTyWith names [] ty ++ "\n") ts
