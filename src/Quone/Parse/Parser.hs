module Quone.Parse.Parser
  ( parseProgram
  ) where

import Data.Char (isLower, isUpper)
import Quone.AST.Source
import Quone.Parse.Token

data Parser = Parser
  { pTokens      :: [Token]
  , pSpans       :: [Span]
  , pPos         :: !Int
  , pIndentGuard :: Maybe (Int, Int)
  }

newParser :: [Token] -> [Span] -> Parser
newParser toks sps = Parser toks sps 0 Nothing

peekTok :: Parser -> Token
peekTok p
  | pPos p < length (pTokens p) = pTokens p !! pPos p
  | otherwise = TEof

spanAt :: Parser -> Span
spanAt p
  | pPos p < length (pSpans p) = pSpans p !! pPos p
  | otherwise = Span maxBound 0

advance :: Parser -> (Token, Parser)
advance p =
  let tok = peekTok p
  in (tok, p { pPos = pPos p + 1 })

err :: Parser -> String -> Either String a
err p msg =
  let s = spanAt p
  in Left $ show (spanLine s) ++ ":" ++ show (spanCol s) ++ ":" ++ msg

expect :: Token -> Parser -> Either String Parser
expect expected p =
  let s = spanAt p
      (tok, p') = advance p
  in if tok == expected
     then Right p'
     else Left $ show (spanLine s) ++ ":" ++ show (spanCol s) ++ ":Expected " ++ show expected ++ ", got " ++ show tok

parseIdent :: Parser -> Either String (String, Parser)
parseIdent p = case advance p of
  (TIdent s, p') -> Right (s, p')
  (tok, _) ->
    let s = spanAt p
    in Left $ show (spanLine s) ++ ":" ++ show (spanCol s) ++ ":Expected identifier, got " ++ show tok

isGuarded :: Parser -> Bool
isGuarded p = case pIndentGuard p of
  Nothing -> False
  Just (guardCol, guardLine) ->
    let s = spanAt p
    in spanCol s <= guardCol && spanLine s > guardLine

isPrimaryStart :: Parser -> Bool
isPrimaryStart p = case peekTok p of
  TIntLit{}    -> True; TFloatLit{} -> True; TTrue -> True; TFalse -> True
  TStringLit{} -> True; TIdent{}    -> True; TLParen -> True; TLBrace -> True
  TDataframe   -> True; TSelect     -> True; TFilter -> True; TMutate -> True
  TSummarize   -> True; TGroupBy    -> True; TArrange -> True; TLBracket -> True
  _ -> False

isAtomicPatternStart :: Parser -> Bool
isAtomicPatternStart p = case peekTok p of
  TIntLit{} -> True; TTrue -> True; TFalse -> True
  TIdent{}  -> True; TUnderscore -> True; TLParen -> True
  _ -> False

isPatternStart :: Parser -> Bool
isPatternStart = isAtomicPatternStart

isLowerIdent :: String -> Bool
isLowerIdent (c:_) = isLower c
isLowerIdent _     = False

isUpperIdent :: String -> Bool
isUpperIdent (c:_) = isUpper c
isUpperIdent _     = False

-- Program

parseProgram :: [Token] -> [Span] -> Either String Program
parseProgram toks sps = do
  let p = newParser toks sps
  (mmod, p1) <- if peekTok p == TPackage
                then do (m, p') <- parseModuleDecl p
                        return (Just m, p')
                else return (Nothing, p)
  (decls, _) <- parseDecls p1
  return $ Program mmod decls

parseDecls :: Parser -> Either String ([Decl], Parser)
parseDecls p
  | peekTok p == TEof = Right ([], p)
  | otherwise = do
      (d, p') <- parseDecl p
      (ds, p'') <- parseDecls p'
      return (d : ds, p'')

parseModuleDecl :: Parser -> Either String (ModuleDecl, Parser)
parseModuleDecl p0 = do
  p1 <- expect TPackage p0
  (name, p2) <- parseDottedName p1
  p3 <- expect TExporting p2
  p4 <- expect TLParen p3
  (exports, p5) <- if peekTok p4 == TDotDot
                    then let (_, p4') = advance p4 in return (ExportAll, p4')
                    else do (ns, p4') <- parseIdentList p4
                            return (ExportList ns, p4')
  p6 <- expect TRParen p5
  return (ModuleDecl name exports, p6)

parseDottedName :: Parser -> Either String (String, Parser)
parseDottedName p = do
  (n, p1) <- parseIdent p
  go n p1
  where
    go acc px
      | peekTok px == TDot =
        let (_, px1) = advance px
        in do (part, px2) <- parseIdent px1
              go (acc ++ "." ++ part) px2
      | otherwise = Right (acc, px)

parseIdentList :: Parser -> Either String ([String], Parser)
parseIdentList p = do
  (n, p1) <- parseIdent p
  go [n] p1
  where
    go acc px
      | peekTok px == TComma =
        let (_, px1) = advance px
        in do (n, px2) <- parseIdent px1
              go (acc ++ [n]) px2
      | otherwise = Right (acc, px)

-- Declarations

parseDecl :: Parser -> Either String (Decl, Parser)
parseDecl p = case peekTok p of
  TType ->
    if pPos p + 1 < length (pTokens p) && pTokens p !! (pPos p + 1) == TAlias
    then parseTypeAliasDecl p
    else parseTypeDecl p
  TImport -> parseExternDecl p
  TLibrary ->
    let (_, p1) = advance p
    in do (name, p2) <- parseIdent p1
          return (DeclLibrary name, p2)
  TIdent{} ->
    let hasColon = pPos p + 1 < length (pTokens p) && pTokens p !! (pPos p + 1) == TColon
    in if hasColon
       then do (ann, p1) <- parseTypeSig p
               (d, p2) <- parseValueDecl (Just ann) p1
               return (d, p2)
       else do (d, p1) <- parseValueDecl Nothing p
               return (d, p1)
  _ -> err p $ "Expected declaration, got " ++ show (peekTok p)

parseTypeSig :: Parser -> Either String ([TypeExpr], Parser)
parseTypeSig p = do
  (_, p1) <- parseIdent p
  p2 <- expect TColon p1
  (first, p3) <- parseTypeSigAtom p2
  parseArrowChain [first] p3

parseArrowChain :: [TypeExpr] -> Parser -> Either String ([TypeExpr], Parser)
parseArrowChain acc p
  | peekTok p == TArrow =
    let (_, p1) = advance p
    in do (te, p2) <- parseTypeSigAtom p1
          parseArrowChain (acc ++ [te]) p2
  | otherwise = Right (acc, p)

-- Type alias declarations

parseTypeAliasDecl :: Parser -> Either String (Decl, Parser)
parseTypeAliasDecl p0 = do
  p1 <- expect TType p0
  p2 <- expect TAlias p1
  (name, p3) <- parseIdent p2
  (params, p4) <- parseLowerParams p3
  p5 <- expect TBind p4
  (first, p6) <- parseTypeSigAtom p5
  (body, p7) <- parseArrowChain [first] p6
  return (DeclTypeAlias (TypeAliasDecl name params body), p7)

-- Extern declarations

parseExternDecl :: Parser -> Either String (Decl, Parser)
parseExternDecl p0 = do
  p1 <- expect TImport p0
  (name, p2) <- parseIdent p1
  p3 <- expect TColon p2
  (first, p4) <- parseTypeSigAtom p3
  (parts, p5) <- parseArrowChain [first] p4
  return (DeclImport (ExternDecl name parts), p5)

parseTypeSigAtom :: Parser -> Either String (TypeExpr, Parser)
parseTypeSigAtom p
  | peekTok p == TLParen = do
      let (_, p1) = advance p
      (first, p2) <- parseTypeSigAtom p1
      (parts, p3) <- parseArrowChain [first] p2
      p4 <- expect TRParen p3
      case parts of
        [single] -> return (single, p4)
        _ -> let ret = last parts
                 params = init parts
             in return (TEFunc params ret, p4)
  | peekTok p == TLBrace = parseRecordTypeSig p
  | peekTok p == TDataframe = do
      let (_, p1) = advance p
      (inner, p2) <- parseRecordTypeSig p1
      case inner of
        TERecord fields -> return (TEDataframe fields, p2)
        _               -> return (inner, p2)
  | otherwise = do
      let sigLine = spanLine (spanAt p)
      (name, p1) <- parseIdent p
      if isLowerIdent name
        then return (TEVar name, p1)
        else do (args, p2) <- parseTypeArgs sigLine p1
                return (TENamed name args, p2)

parseTypeArgs :: Int -> Parser -> Either String ([TypeExpr], Parser)
parseTypeArgs sigLine p
  | spanLine (spanAt p) == sigLine = case peekTok p of
      TIdent s | isUpperIdent s -> do
        (name, p1) <- parseIdent p
        (rest, p2) <- parseTypeArgs sigLine p1
        return (TENamed name [] : rest, p2)
      TIdent s | isLowerIdent s -> do
        (name, p1) <- parseIdent p
        (rest, p2) <- parseTypeArgs sigLine p1
        return (TEVar name : rest, p2)
      _ -> return ([], p)
  | otherwise = return ([], p)

parseRecordTypeSig :: Parser -> Either String (TypeExpr, Parser)
parseRecordTypeSig p0 = do
  p1 <- expect TLBrace p0
  (fields, p2) <- parseRecordFields' p1
  p3 <- expect TRBrace p2
  return (TERecord fields, p3)
  where
    parseRecordFields' px
      | peekTok px == TRBrace = return ([], px)
      | otherwise = do
          (name, px1) <- parseIdent px
          px2 <- expect TColon px1
          (first, px3) <- parseTypeSigAtom px2
          (parts, px4) <- parseArrowChain [first] px3
          let fieldTy = case parts of
                [single] -> single
                _        -> TEFunc (init parts) (last parts)
          case peekTok px4 of
            TComma -> do
              let (_, px5) = advance px4
              (rest, px6) <- parseRecordFields' px5
              return ((name, fieldTy) : rest, px6)
            _ -> return ([(name, fieldTy)], px4)

-- Type declarations

parseTypeDecl :: Parser -> Either String (Decl, Parser)
parseTypeDecl p0 = do
  p1 <- expect TType p0
  (name, p2) <- parseIdent p1
  (params, p3) <- parseLowerParams p2
  p4 <- expect TBind p3
  let p5 = if peekTok p4 == TPipe then let (_, px) = advance p4 in px else p4
  (first, p6) <- parseVariant p5
  (rest, p7) <- parseVariants p6
  return (DeclType (TypeDecl name params (first : rest)), p7)

parseVariants :: Parser -> Either String ([VariantDecl], Parser)
parseVariants p
  | peekTok p == TPipe = do
      let (_, p1) = advance p
      (v, p2) <- parseVariant p1
      (vs, p3) <- parseVariants p2
      return (v : vs, p3)
  | otherwise = return ([], p)

parseVariant :: Parser -> Either String (VariantDecl, Parser)
parseVariant p = do
  let vLine = spanLine (spanAt p)
  (name, p1) <- parseIdent p
  (fields, p2) <- parseVariantFields vLine p1
  return (VariantDecl name fields, p2)

parseVariantFields :: Int -> Parser -> Either String ([TypeExpr], Parser)
parseVariantFields vLine p = case peekTok p of
  TIdent{} | spanLine (spanAt p) == vLine -> do
    (te, p1) <- parseTypeAtom p
    (rest, p2) <- parseVariantFields vLine p1
    return (te : rest, p2)
  TLParen | spanLine (spanAt p) == vLine -> do
    (te, p1) <- parseTypeAtom p
    (rest, p2) <- parseVariantFields vLine p1
    return (te : rest, p2)
  _ -> return ([], p)

parseTypeAtom :: Parser -> Either String (TypeExpr, Parser)
parseTypeAtom p = case peekTok p of
  TLParen -> do
    let (_, p1) = advance p
    (te, p2) <- parseTypeExpr p1
    p3 <- expect TRParen p2
    return (te, p3)
  TIdent s -> do
    (name, p1) <- parseIdent p
    if isLowerIdent s
      then return (TEVar name, p1)
      else return (TENamed name [], p1)
  _ -> err p $ "Expected type, got " ++ show (peekTok p)

parseTypeExpr :: Parser -> Either String (TypeExpr, Parser)
parseTypeExpr p = do
  (name, p1) <- parseIdent p
  if isLowerIdent name
    then return (TEVar name, p1)
    else do (args, p2) <- parseLowerTypeArgs p1
            return (TENamed name args, p2)

parseLowerTypeArgs :: Parser -> Either String ([TypeExpr], Parser)
parseLowerTypeArgs p = case peekTok p of
  TIdent s | isLowerIdent s -> do
    (name, p1) <- parseIdent p
    (rest, p2) <- parseLowerTypeArgs p1
    return (TEVar name : rest, p2)
  _ -> return ([], p)

parseLowerParams :: Parser -> Either String ([String], Parser)
parseLowerParams p = case peekTok p of
  TIdent s | isLowerIdent s -> do
    (name, p1) <- parseIdent p
    (rest, p2) <- parseLowerParams p1
    return (name : rest, p2)
  _ -> return ([], p)

-- Value declarations

parseValueDecl :: Maybe [TypeExpr] -> Parser -> Either String (Decl, Parser)
parseValueDecl ann p = do
  let declCol = spanCol (spanAt p)
  (name, p1) <- parseIdent p
  (params, p2) <- parseLowerParams p1
  let eqLine = spanLine (spanAt p2)
  p3 <- expect TBind p2
  let prevGuard = pIndentGuard p3
      p4 = p3 { pIndentGuard = Just (declCol, eqLine) }
  (body, p5) <- parseExpr p4
  let p6 = p5 { pIndentGuard = prevGuard }
      expr = if null params then body else ELambda params body
  return (DeclLet (LetDecl name expr ann), p6)

-- Expressions

parseExpr :: Parser -> Either String (Expr, Parser)
parseExpr p = case peekTok p of
  TBackslash -> parseLambda p
  TIf        -> parseIf p
  TCase      -> parseMatch p
  TLet       -> parseLetExpr p
  _          -> parsePipe p

parseParamName :: Parser -> Either String (String, Parser)
parseParamName p
  | peekTok p == TUnderscore = let (_, p1) = advance p in Right ("_", p1)
  | otherwise = parseIdent p

parseLambda :: Parser -> Either String (Expr, Parser)
parseLambda p0 = do
  p1 <- expect TBackslash p0
  (first, p2) <- parseParamName p1
  (rest, p3) <- parseLambdaParams p2
  p4 <- expect TArrow p3
  (body, p5) <- parseExpr p4
  return (ELambda (first : rest) body, p5)

parseLambdaParams :: Parser -> Either String ([String], Parser)
parseLambdaParams p = case peekTok p of
  TIdent s | isLowerIdent s -> do
    (name, p1) <- parseParamName p
    (rest, p2) <- parseLambdaParams p1
    return (name : rest, p2)
  TUnderscore -> do
    (name, p1) <- parseParamName p
    (rest, p2) <- parseLambdaParams p1
    return (name : rest, p2)
  _ -> return ([], p)

parseIf :: Parser -> Either String (Expr, Parser)
parseIf p0 = do
  p1 <- expect TIf p0
  (cond, p2) <- parseExpr p1
  p3 <- expect TThen p2
  (thenE, p4) <- parseExpr p3
  p5 <- expect TElse p4
  (elseE, p6) <- parseExpr p5
  return (EIf cond thenE elseE, p6)

parseMatch :: Parser -> Either String (Expr, Parser)
parseMatch p0 = do
  p1 <- expect TCase p0
  (scrutinee, p2) <- parseEquality p1
  p3 <- expect TOf p2
  let armCol = spanCol (spanAt p3)
      prevGuard = pIndentGuard p3
  (arms, p4) <- parseMatchArms armCol prevGuard p3
  let p5 = p4 { pIndentGuard = prevGuard }
  if null arms
    then Left "Expected at least one case arm"
    else return (EMatch scrutinee arms, p5)

parseMatchArms :: Int -> Maybe (Int, Int) -> Parser -> Either String ([(Pattern, Expr)], Parser)
parseMatchArms armCol prevGuard p
  | peekTok p == TEof = return ([], p)
  | spanCol (spanAt p) == armCol && isPatternStart p = do
      (pat, p1) <- parsePattern p
      let arrowLine = spanLine (spanAt p1)
      p2 <- expect TArrow p1
      let p3 = p2 { pIndentGuard = Just (armCol, arrowLine) }
      (body, p4) <- parseExpr p3
      let p5 = p4 { pIndentGuard = prevGuard }
      (rest, p6) <- parseMatchArms armCol prevGuard p5
      return ((pat, body) : rest, p6)
  | otherwise = return ([], p)

parseLetExpr :: Parser -> Either String (Expr, Parser)
parseLetExpr p0 = do
  p1 <- expect TLet p0
  let bindCol = spanCol (spanAt p1)
      prevGuard = pIndentGuard p1
  (bindings, p2) <- parseLetBindings bindCol prevGuard p1
  let p3 = p2 { pIndentGuard = prevGuard }
  p4 <- expect TIn p3
  (body, p5) <- parseExpr p4
  if null bindings
    then Left "Expected at least one let binding"
    else return (ELet bindings body, p5)

parseLetBindings :: Int -> Maybe (Int, Int) -> Parser -> Either String ([(String, Expr)], Parser)
parseLetBindings bindCol prevGuard p
  | peekTok p == TIn || peekTok p == TEof = return ([], p)
  | spanCol (spanAt p) == bindCol && (isIdentTok (peekTok p) || peekTok p == TUnderscore) = do
      (name0, p1) <- parseParamName p
      let name = if peekTok p1 == TDot
                 then let (_, p1') = advance p1
                      in case parseIdent p1' of
                           Right (field, _) -> name0 ++ "." ++ field
                           Left _ -> name0
                 else name0
      let p1' = if '.' `elem` name && peekTok p1 == TDot
                then let (_, px) = advance p1
                     in case advance px of (_, px') -> px'
                else p1
      (params, p2) <- if name /= "_" && '.' `notElem` name
                      then parseLowerParams p1'
                      else return ([], p1')
      let eqLine = spanLine (spanAt p2)
      p3 <- expect TBind p2
      let p4 = p3 { pIndentGuard = Just (bindCol, eqLine) }
      (bodyE, p5) <- parseExpr p4
      let p6 = p5 { pIndentGuard = Just (bindCol, eqLine) }
      let expr = if null params then bodyE else ELambda params bodyE
      (rest, p7) <- parseLetBindings bindCol prevGuard p6
      return ((name, expr) : rest, p7)
  | otherwise = return ([], p)
  where
    isIdentTok (TIdent _) = True
    isIdentTok _ = False

-- Patterns

parsePattern :: Parser -> Either String (Pattern, Parser)
parsePattern p = case peekTok p of
  TUnderscore -> let (_, p1) = advance p in Right (PWildcard, p1)
  TTrue  -> let (_, p1) = advance p in Right (PBool True, p1)
  TFalse -> let (_, p1) = advance p in Right (PBool False, p1)
  TIntLit n -> let (_, p1) = advance p in Right (PInt n, p1)
  TIdent name | isUpperIdent name -> do
    let (_, p1) = advance p
    (subs, p2) <- parseAtomicPatterns p1
    return (PConstructor name subs, p2)
  TIdent name -> let (_, p1) = advance p in Right (PVar name, p1)
  TLParen -> do
    let (_, p1) = advance p
    (pat, p2) <- parsePattern p1
    p3 <- expect TRParen p2
    return (pat, p3)
  _ -> err p $ "Expected pattern, got " ++ show (peekTok p)

parseAtomicPatterns :: Parser -> Either String ([Pattern], Parser)
parseAtomicPatterns p
  | isAtomicPatternStart p && not (isGuarded p) = do
      (pat, p1) <- parseAtomicPattern p
      (rest, p2) <- parseAtomicPatterns p1
      return (pat : rest, p2)
  | otherwise = return ([], p)

parseAtomicPattern :: Parser -> Either String (Pattern, Parser)
parseAtomicPattern p = case peekTok p of
  TUnderscore -> let (_, p1) = advance p in Right (PWildcard, p1)
  TTrue  -> let (_, p1) = advance p in Right (PBool True, p1)
  TFalse -> let (_, p1) = advance p in Right (PBool False, p1)
  TIntLit n -> let (_, p1) = advance p in Right (PInt n, p1)
  TIdent name | isUpperIdent name -> let (_, p1) = advance p in Right (PConstructor name [], p1)
  TIdent name -> let (_, p1) = advance p in Right (PVar name, p1)
  TLParen -> do
    let (_, p1) = advance p
    (pat, p2) <- parsePattern p1
    p3 <- expect TRParen p2
    return (pat, p3)
  _ -> err p $ "Expected pattern, got " ++ show (peekTok p)

-- Precedence climbing

parsePipe :: Parser -> Either String (Expr, Parser)
parsePipe p = do
  (left, p1) <- parseEquality p
  parsePipeRest left p1

parsePipeRest :: Expr -> Parser -> Either String (Expr, Parser)
parsePipeRest left p
  | not (isGuarded p) && peekTok p == TPipeRight = do
      let (_, p1) = advance p
      (right, p2) <- parseEquality p1
      parsePipeRest (EPipe left right) p2
  | otherwise = return (left, p)

parseEquality :: Parser -> Either String (Expr, Parser)
parseEquality p = do
  (left, p1) <- parseAdditive p
  parseEqualityRest left p1

parseEqualityRest :: Expr -> Parser -> Either String (Expr, Parser)
parseEqualityRest left p
  | isGuarded p = return (left, p)
  | otherwise = case peekTok p of
      TEqEq -> binStep Eq
      TNeq  -> binStep Neq
      TGt   -> binStep Gt
      TLt   -> binStep Lt
      TGtEq -> binStep GtEq
      TLtEq -> binStep LtEq
      _     -> return (left, p)
  where
    binStep op = do
      let (_, p1) = advance p
      (right, p2) <- parseAdditive p1
      parseEqualityRest (EBinary op left right) p2

parseAdditive :: Parser -> Either String (Expr, Parser)
parseAdditive p = do
  (left, p1) <- parseMultiplicative p
  parseAdditiveRest left p1

parseAdditiveRest :: Expr -> Parser -> Either String (Expr, Parser)
parseAdditiveRest left p
  | isGuarded p = return (left, p)
  | otherwise = case peekTok p of
      TPlus  -> do
        let (_, p1) = advance p
        (right, p2) <- parseMultiplicative p1
        parseAdditiveRest (EBinary Add left right) p2
      TMinus -> do
        let (_, p1) = advance p
        (right, p2) <- parseMultiplicative p1
        parseAdditiveRest (EBinary Sub left right) p2
      _ -> return (left, p)

parseMultiplicative :: Parser -> Either String (Expr, Parser)
parseMultiplicative p = do
  (left, p1) <- parseApplication p
  parseMultiplicativeRest left p1

parseMultiplicativeRest :: Expr -> Parser -> Either String (Expr, Parser)
parseMultiplicativeRest left p
  | isGuarded p = return (left, p)
  | otherwise = case peekTok p of
      TStar  -> do
        let (_, p1) = advance p
        (right, p2) <- parseApplication p1
        parseMultiplicativeRest (EBinary Mul left right) p2
      TSlash -> do
        let (_, p1) = advance p
        (right, p2) <- parseApplication p1
        parseMultiplicativeRest (EBinary Div left right) p2
      _ -> return (left, p)

parsePostfix :: Parser -> Either String (Expr, Parser)
parsePostfix p = do
  (expr, p1) <- parsePrimary p
  parsePostfixRest expr p1

parsePostfixRest :: Expr -> Parser -> Either String (Expr, Parser)
parsePostfixRest expr p
  | peekTok p == TDot = do
      let (_, p1) = advance p
      (field, p2) <- parseIdent p1
      parsePostfixRest (EFieldAccess expr field) p2
  | otherwise = return (expr, p)

parseApplication :: Parser -> Either String (Expr, Parser)
parseApplication p = do
  (func, p1) <- parsePostfix p
  (args, p2) <- parseAppArgs p1
  if null args
    then return (func, p2)
    else return (ECall func args, p2)

parseAppArgs :: Parser -> Either String ([Expr], Parser)
parseAppArgs p
  | not (isGuarded p) && isPrimaryStart p = do
      (arg, p1) <- parsePostfix p
      (rest, p2) <- parseAppArgs p1
      return (arg : rest, p2)
  | otherwise = return ([], p)

parsePrimary :: Parser -> Either String (Expr, Parser)
parsePrimary p = case peekTok p of
  TIntLit n    -> let (_, p1) = advance p in Right (EInt n, p1)
  TFloatLit f  -> let (_, p1) = advance p in Right (EFloat f, p1)
  TTrue        -> let (_, p1) = advance p in Right (EBool True, p1)
  TFalse       -> let (_, p1) = advance p in Right (EBool False, p1)
  TStringLit s -> let (_, p1) = advance p in Right (EStr s, p1)
  TIdent name  -> let (_, p1) = advance p in Right (EVar name, p1)
  TLParen -> do
    let (_, p1) = advance p
    if peekTok p1 == TRParen
      then let (_, p2) = advance p1 in return (EUnit, p2)
      else do (expr, p2) <- parseExpr p1
              p3 <- expect TRParen p2
              return (expr, p3)
  TLBrace -> do
    let (_, p1) = advance p
    (fields, p2) <- parseRecordFields p1
    p3 <- expect TRBrace p2
    return (ERecord fields, p3)
  TDataframe -> do
    let (_, p1) = advance p
    p2 <- expect TLBrace p1
    (fields, p3) <- parseRecordFields p2
    p4 <- expect TRBrace p3
    return (EDataFrame fields, p4)
  TLBracket -> do
    let (_, p1) = advance p
    (elems, p2) <- parseExprList p1
    p3 <- expect TRBracket p2
    return (EVecLit elems, p3)
  TSelect  -> parseDplyrCols "select" p
  TGroupBy -> parseDplyrCols "group_by" p
  TArrange -> do
    let (_, p1) = advance p
    (arg, p2) <- parsePrimary p1
    return (EDplyr "arrange" [DplyrPred arg], p2)
  TFilter -> do
    let (_, p1) = advance p
    (pred', p2) <- parsePrimary p1
    return (EDplyr "filter" [DplyrPred pred'], p2)
  TMutate    -> parseDplyrNamed "mutate" p
  TSummarize -> parseDplyrNamed "summarize" p
  _ -> err p $ "Expected expression, got " ++ show (peekTok p)

parseRecordFields :: Parser -> Either String ([(String, Expr)], Parser)
parseRecordFields p
  | peekTok p == TRBrace = return ([], p)
  | otherwise = do
      (name, p1) <- parseIdent p
      p2 <- expect TEquals p1
      (val, p3) <- parseExpr p2
      case peekTok p3 of
        TComma -> do
          let (_, p4) = advance p3
          (rest, p5) <- parseRecordFields p4
          return ((name, val) : rest, p5)
        _ -> return ([(name, val)], p3)

parseExprList :: Parser -> Either String ([Expr], Parser)
parseExprList p
  | peekTok p == TRBracket = return ([], p)
  | otherwise = do
      (e, p1) <- parseExpr p
      case peekTok p1 of
        TComma -> do
          let (_, p2) = advance p1
          (rest, p3) <- parseExprList p2
          return (e : rest, p3)
        _ -> return ([e], p1)

parseDplyrCols :: String -> Parser -> Either String (Expr, Parser)
parseDplyrCols verb p = do
  let (_, p1) = advance p
      startLine = spanLine (spanAt p1)
  (first, p2) <- parseIdent p1
  (rest, p3) <- parseDplyrColsRest startLine p2
  return (EDplyr verb (map DplyrColumn (first : rest)), p3)

parseDplyrColsRest :: Int -> Parser -> Either String ([String], Parser)
parseDplyrColsRest startLine p = case peekTok p of
  TIdent{} | spanLine (spanAt p) == startLine -> do
    (name, p1) <- parseIdent p
    (rest, p2) <- parseDplyrColsRest startLine p1
    return (name : rest, p2)
  _ -> return ([], p)

parseDplyrNamed :: String -> Parser -> Either String (Expr, Parser)
parseDplyrNamed verb p = do
  let (_, p1) = advance p
  p2 <- expect TLBrace p1
  (args, p3) <- parseDplyrNamedFields p2
  p4 <- expect TRBrace p3
  return (EDplyr verb args, p4)

parseDplyrNamedFields :: Parser -> Either String ([DplyrArg], Parser)
parseDplyrNamedFields p
  | peekTok p == TRBrace = return ([], p)
  | otherwise = do
      (name, p1) <- parseIdent p
      p2 <- expect TEquals p1
      (val, p3) <- parseExpr p2
      case peekTok p3 of
        TComma -> do
          let (_, p4) = advance p3
          (rest, p5) <- parseDplyrNamedFields p4
          return (DplyrNamed name val : rest, p5)
        _ -> return ([DplyrNamed name val], p3)
