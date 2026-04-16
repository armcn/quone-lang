module Quone.Format
  ( formatSource
  ) where

import Data.List (intercalate)

import Quone.AST.Source
import Quone.Parse.Lexer (tokenize)
import Quone.Parse.Parser (parseProgram)

formatSource :: String -> Either String String
formatSource source = do
  (toks, sps) <- tokenize source
  prog <- parseProgram toks sps
  let headerComments = extractHeaderComments source
      out = if null headerComments
            then fmtProgram prog
            else unlines headerComments ++ "\n" ++ fmtProgram prog
  Right out

extractHeaderComments :: String -> [String]
extractHeaderComments source = go (lines source)
  where
    go [] = []
    go (l:ls)
      | isComment (strip l) = stripEnd l : go ls
      | null (strip l) = go ls
      | otherwise = []
    isComment ('#':_) = True
    isComment _       = False
    strip = dropWhile (== ' ')
    stripEnd = reverse . dropWhile (== ' ') . reverse

fmtProgram :: Program -> String
fmtProgram prog =
  let sections = concat
        [ maybe [] (\m -> [fmtModuleDecl m]) (progModule prog)
        , let libs = [n | DeclLibrary n <- progDecls prog]
          in if null libs then [] else [unlines (map (\l -> "library " ++ l) libs)]
        , let imps = [e | DeclImport e <- progDecls prog]
          in if null imps then [] else [unlines (map fmtImport imps)]
        , [fmtDecl d | d <- progDecls prog, isRestDecl d]
        ]
      body = intercalate "\n\n" sections
  in if null body || last body == '\n' then body else body ++ "\n"
  where
    isRestDecl (DeclLibrary _) = False
    isRestDecl (DeclImport _)  = False
    isRestDecl _               = True

fmtModuleDecl :: ModuleDecl -> String
fmtModuleDecl m = case moduleExports m of
  ExportAll     -> "package " ++ moduleName m ++ " exporting (..)"
  ExportList ns -> "package " ++ moduleName m ++ " exporting (" ++ intercalate ", " ns ++ ")"

fmtImport :: ExternDecl -> String
fmtImport e = "import " ++ externName e ++ " : " ++ fmtTypeExprs (externTyExpr e)

fmtTypeExprs :: [TypeExpr] -> String
fmtTypeExprs [TEFunc params ret] =
  intercalate " -> " (map fmtTypeExpr params ++ [fmtTypeExpr ret])
fmtTypeExprs [single] = fmtTypeExpr single
fmtTypeExprs exprs = intercalate " -> " (map fmtTypeExpr exprs)

fmtTypeExpr :: TypeExpr -> String
fmtTypeExpr te = case te of
  TEVar v -> v
  TENamed name [] -> name
  TENamed name args ->
    let argStrs = map (\a -> case a of
          TENamed _ (_:_) -> "(" ++ fmtTypeExpr a ++ ")"
          _               -> fmtTypeExpr a
          ) args
    in name ++ " " ++ unwords argStrs
  TEFunc params ret ->
    "(" ++ intercalate " -> " (map fmtTypeExpr params ++ [fmtTypeExpr ret]) ++ ")"
  TERecord fields -> fmtRecordType fields
  TEDataframe fields -> fmtDataframeType fields

fmtRecordType :: [(String, TypeExpr)] -> String
fmtRecordType [] = "{}"
fmtRecordType fields
  | length fields <= 2 =
    let inner = intercalate ", " [n ++ " : " ++ fmtTypeExpr t | (n, t) <- fields]
    in "{ " ++ inner ++ " }"
  | otherwise =
    let ls = zipWith (\i (n, t) ->
              if i == (0 :: Int)
              then "    { " ++ n ++ " : " ++ fmtTypeExpr t
              else "    , " ++ n ++ " : " ++ fmtTypeExpr t
              ) [0..] fields
    in unlines (ls ++ ["    }"])

fmtDataframeType :: [(String, TypeExpr)] -> String
fmtDataframeType [] = "dataframe {}"
fmtDataframeType fields
  | length fields <= 2 =
    let inner = intercalate ", " [n ++ " : " ++ fmtTypeExpr t | (n, t) <- fields]
    in "dataframe { " ++ inner ++ " }"
  | otherwise =
    let ls = "    dataframe" : zipWith (\i (n, t) ->
              if i == (0 :: Int)
              then "        { " ++ n ++ " : " ++ fmtTypeExpr t
              else "        , " ++ n ++ " : " ++ fmtTypeExpr t
              ) [0..] fields
    in unlines (ls ++ ["        }"])

fmtDecl :: Decl -> String
fmtDecl d = case d of
  DeclType td       -> fmtTypeDecl td
  DeclTypeAlias ta  -> fmtTypeAlias ta
  DeclLet ld        -> fmtLetDecl ld
  DeclImport e      -> fmtImport e
  DeclLibrary name  -> "library " ++ name

fmtTypeDecl :: TypeDecl -> String
fmtTypeDecl td =
  let header = if null (typeParams td)
               then "type " ++ typeName td
               else "type " ++ typeName td ++ " " ++ unwords (typeParams td)
      ls = zipWith (\i v ->
            let fieldsStr = if null (variantFields v)
                            then ""
                            else " " ++ unwords (map fmtTypeExpr (variantFields v))
            in if i == (0 :: Int)
               then header ++ "\n    <- " ++ variantName v ++ fieldsStr
               else "    | " ++ variantName v ++ fieldsStr
            ) [0..] (typeVariants td)
  in unlines ls

fmtTypeAlias :: TypeAliasDecl -> String
fmtTypeAlias ta =
  let header = if null (aliasParams ta)
               then "type alias " ++ aliasName ta ++ " <-"
               else "type alias " ++ aliasName ta ++ " " ++ unwords (aliasParams ta) ++ " <-"
  in case aliasBody ta of
    [TERecord fields] | length fields > 2 -> header ++ "\n" ++ fmtRecordType fields
    [TEDataframe fields] | length fields > 2 -> header ++ "\n" ++ fmtDataframeType fields
    _ -> header ++ " " ++ fmtTypeExprs (aliasBody ta)

fmtLetDecl :: LetDecl -> String
fmtLetDecl ld =
  let annStr = case letAnnotation ld of
        Just ann -> letName ld ++ " : " ++ fmtTypeExprs ann ++ "\n"
        Nothing  -> ""
  in annStr ++ case letExpr ld of
    ELambda params body ->
      letName ld ++ " " ++ unwords params ++ " <-\n" ++ fmtBody body 4
    _ ->
      if isSimpleExpr (letExpr ld)
      then letName ld ++ " <-\n    " ++ fmtExprInline (letExpr ld)
      else letName ld ++ " <-\n" ++ fmtExprIndented (letExpr ld) 4

isSimpleExpr :: Expr -> Bool
isSimpleExpr e = case e of
  EVar{} -> True; EInt{} -> True; EFloat{} -> True; EBool{} -> True; EStr{} -> True; EUnit -> True
  EVecLit elems -> length elems <= 5 && all isSimpleExpr elems
  ECall f args -> isSimpleExpr f && all isSimpleExpr args && length args <= 3
  ERecord fields -> length fields <= 3 && all (isSimpleExpr . snd) fields
  EDataFrame fields -> length fields <= 3 && all (isSimpleExpr . snd) fields
  EBinary _ l r -> isSimpleExpr l && isSimpleExpr r
  EFieldAccess e' _ -> isSimpleExpr e'
  _ -> False

fmtBody :: Expr -> Int -> String
fmtBody expr indent =
  let pad = replicate indent ' '
  in case expr of
    ELet bindings body ->
      pad ++ "let\n" ++ concatMap (\(n, v) -> fmtLetBinding n v (indent + 4) ++ "\n") bindings
      ++ pad ++ "in\n" ++ fmtBody body indent
    EIf cond thenE elseE ->
      pad ++ "if " ++ fmtExprInline cond ++ " then\n"
      ++ fmtBody thenE (indent + 4) ++ "\n"
      ++ pad ++ "else\n"
      ++ fmtBody elseE (indent + 4)
    EMatch scrutinee arms ->
      pad ++ "case " ++ fmtExprInline scrutinee ++ " of\n"
      ++ concatMap (\(i, (pat, body)) ->
          (if i > (0 :: Int) then "\n" else "")
          ++ pad ++ "    " ++ fmtPattern pat ++ " ->\n"
          ++ fmtBody body (indent + 8)
        ) (zip [0..] arms)
    EPipe _ _ ->
      let chain = collectPipeChain expr
      in case chain of
        [] -> pad
        (first:rest) ->
          fmtBody first indent ++ concatMap (\step -> "\n" ++ pad ++ "|> " ++ fmtExprInline step) rest
    _ -> pad ++ fmtExprInline expr

collectPipeChain :: Expr -> [Expr]
collectPipeChain (EPipe left right) = collectPipeChain left ++ [right]
collectPipeChain e = [e]

fmtLetBinding :: String -> Expr -> Int -> String
fmtLetBinding name val indent =
  let pad = replicate indent ' '
  in case val of
    ELambda params body | name /= "_" && '.' `notElem` name ->
      pad ++ name ++ " " ++ unwords params ++ " <-\n" ++ fmtBody body (indent + 4)
    _ ->
      if name == "_" || '.' `elem` name
      then pad ++ name ++ " <- " ++ fmtExprInline val
      else pad ++ name ++ " <- " ++ fmtExprInline val

fmtExprIndented :: Expr -> Int -> String
fmtExprIndented e indent = case e of
  EPipe{} -> fmtBody e indent
  ELet{}  -> fmtBody e indent
  EIf{}   -> fmtBody e indent
  EMatch{} -> fmtBody e indent
  _ -> replicate indent ' ' ++ fmtExprInline e

fmtExprInline :: Expr -> String
fmtExprInline e = case e of
  EVar name -> name
  EInt n -> show n
  EFloat f -> let s = show f in if '.' `elem` s then s else s ++ ".0"
  EBool b -> if b then "True" else "False"
  EStr s -> "\"" ++ s ++ "\""
  EUnit -> "()"
  EVecLit elems -> "[" ++ intercalate ", " (map fmtExprInline elems) ++ "]"
  ELambda params body -> "(\\" ++ unwords params ++ " -> " ++ fmtExprInline body ++ ")"
  ECall func args
    | null args || (length args == 1 && isUnitE (head args)) ->
      fmtExprInline func ++ " ()"
    | otherwise ->
      let f = fmtExprInline func
          argStrs = map fmtCallArg (filter (not . isUnitE) args)
      in f ++ " " ++ unwords argStrs
  EIf cond thenE elseE ->
    "if " ++ fmtExprInline cond ++ " then " ++ fmtExprInline thenE ++ " else " ++ fmtExprInline elseE
  EBinary op l r ->
    fmtBinaryChild l op True ++ " " ++ binOpSymbol op ++ " " ++ fmtBinaryChild r op False
  ERecord fields -> fmtRecordLit fields
  EDataFrame fields -> fmtDataframeLit fields
  EFieldAccess e' field -> fmtExprInline e' ++ "." ++ field
  EPipe l r -> fmtExprInline l ++ " |> " ++ fmtExprInline r
  EDplyr verb args -> fmtDplyr verb args
  ELet{} -> fmtBody e 0
  EMatch{} -> fmtBody e 0

isUnitE :: Expr -> Bool
isUnitE EUnit = True
isUnitE _     = False

fmtBinaryChild :: Expr -> BinOp -> Bool -> String
fmtBinaryChild e parentOp isLeft = case e of
  EBinary childOp _ _
    | (isLeft && binOpPrecedence childOp < binOpPrecedence parentOp) ||
      (not isLeft && binOpPrecedence childOp <= binOpPrecedence parentOp) ->
      "(" ++ fmtExprInline e ++ ")"
  _ -> fmtExprInline e

fmtCallArg :: Expr -> String
fmtCallArg e = case e of
  ECall{}   -> "(" ++ fmtExprInline e ++ ")"
  EBinary{} -> "(" ++ fmtExprInline e ++ ")"
  EIf{}     -> "(" ++ fmtExprInline e ++ ")"
  _         -> fmtExprInline e

fmtRecordLit :: [(String, Expr)] -> String
fmtRecordLit fields
  | length fields <= 3 && all (isSimpleExpr . snd) fields =
    let inner = intercalate ", " [n ++ " = " ++ fmtExprInline v | (n, v) <- fields]
    in "{ " ++ inner ++ " }"
  | otherwise =
    let ls = zipWith (\i (n, v) ->
              if i == (0 :: Int)
              then "    { " ++ n ++ " = " ++ fmtExprInline v
              else "    , " ++ n ++ " = " ++ fmtExprInline v
              ) [0..] fields
    in unlines (ls ++ ["    }"])

fmtDataframeLit :: [(String, Expr)] -> String
fmtDataframeLit fields
  | length fields <= 2 && all (isSimpleExpr . snd) fields =
    let inner = intercalate ", " [n ++ " = " ++ fmtExprInline v | (n, v) <- fields]
    in "dataframe { " ++ inner ++ " }"
  | otherwise =
    let ls = "dataframe" : zipWith (\i (n, v) ->
              if i == (0 :: Int)
              then "        { " ++ n ++ " = " ++ fmtExprInline v
              else "        , " ++ n ++ " = " ++ fmtExprInline v
              ) [0..] fields
    in unlines (ls ++ ["        }"])

fmtDplyr :: String -> [DplyrArg] -> String
fmtDplyr verb args = case verb of
  "select"   -> verb ++ " " ++ unwords [n | DplyrColumn n <- args]
  "group_by" -> verb ++ " " ++ unwords [n | DplyrColumn n <- args]
  "arrange"  -> verb ++ " " ++ unwords (map fmtDplyrArg args)
  "filter"   ->
    let preds = [fmtExprInline e | DplyrPred e <- args]
    in verb ++ " " ++ unwords (map (\p -> "(" ++ p ++ ")") preds)
  "mutate"    -> fmtDplyrNamed verb args
  "summarize" -> fmtDplyrNamed verb args
  _ -> verb

fmtDplyrNamed :: String -> [DplyrArg] -> String
fmtDplyrNamed verb args =
  let entries = intercalate ", " (map fmtDplyrArg args)
  in verb ++ " { " ++ entries ++ " }"

fmtDplyrArg :: DplyrArg -> String
fmtDplyrArg (DplyrColumn name) = name
fmtDplyrArg (DplyrPred expr) = "(" ++ fmtExprInline expr ++ ")"
fmtDplyrArg (DplyrNamed name expr) = name ++ " = " ++ fmtExprInline expr

fmtPattern :: Pattern -> String
fmtPattern pat = case pat of
  PWildcard -> "_"
  PVar name -> name
  PInt n -> show n
  PBool b -> if b then "True" else "False"
  PConstructor name fields
    | null fields -> name
    | otherwise -> name ++ " " ++ unwords (map fmtPattern fields)
