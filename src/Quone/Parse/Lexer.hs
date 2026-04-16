module Quone.Parse.Lexer
  ( tokenize
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Quone.Parse.Token

data Lexer = Lexer
  { lexChars :: String
  , lexPos   :: !Int
  , lexLine  :: !Int
  , lexCol   :: !Int
  }

newLexer :: String -> Lexer
newLexer input = Lexer input 0 1 1

peek :: Lexer -> Maybe Char
peek l
  | lexPos l < length (lexChars l) = Just (lexChars l !! lexPos l)
  | otherwise = Nothing

advanceChar :: Lexer -> Lexer
advanceChar l
  | lexPos l >= length (lexChars l) = l
  | lexChars l !! lexPos l == '\n'  = l { lexPos = lexPos l + 1, lexLine = lexLine l + 1, lexCol = 1 }
  | otherwise                       = l { lexPos = lexPos l + 1, lexCol = lexCol l + 1 }

skipWhitespaceAndComments :: Lexer -> Lexer
skipWhitespaceAndComments l = case peek l of
  Nothing -> l
  Just ch
    | isSpace ch -> skipWhitespaceAndComments (advanceChar l)
    | ch == '#'  -> skipWhitespaceAndComments (skipLine l)
    | otherwise  -> l
  where
    skipLine lx = case peek lx of
      Nothing   -> lx
      Just '\n' -> advanceChar lx
      Just _    -> skipLine (advanceChar lx)

currentSpan :: Lexer -> Span
currentSpan l = Span (lexLine l) (lexCol l)

sliceChars :: Lexer -> Int -> Int -> String
sliceChars l start end = take (end - start) (drop start (lexChars l))

nextToken :: Lexer -> Either String (Token, Span, Lexer)
nextToken l0 =
  let l = skipWhitespaceAndComments l0
      sp = currentSpan l
  in case peek l of
    Nothing -> Right (TEof, sp, l)
    Just ch -> case ch of
      '(' -> Right (TLParen, sp, advanceChar l)
      ')' -> Right (TRParen, sp, advanceChar l)
      '{' -> Right (TLBrace, sp, advanceChar l)
      '}' -> Right (TRBrace, sp, advanceChar l)
      '[' -> Right (TLBracket, sp, advanceChar l)
      ']' -> Right (TRBracket, sp, advanceChar l)
      ',' -> Right (TComma, sp, advanceChar l)
      '.' -> let l1 = advanceChar l
             in case peek l1 of
                  Just '.' -> Right (TDotDot, sp, advanceChar l1)
                  _        -> Right (TDot, sp, l1)
      ':' -> Right (TColon, sp, advanceChar l)
      '|' -> let l1 = advanceChar l
             in case peek l1 of
                  Just '>' -> Right (TPipeRight, sp, advanceChar l1)
                  _        -> Right (TPipe, sp, l1)
      '+' -> Right (TPlus, sp, advanceChar l)
      '*' -> Right (TStar, sp, advanceChar l)
      '/' -> Right (TSlash, sp, advanceChar l)
      '\\' -> Right (TBackslash, sp, advanceChar l)
      '-' -> let l1 = advanceChar l
             in case peek l1 of
                  Just '>' -> Right (TArrow, sp, advanceChar l1)
                  _        -> Right (TMinus, sp, l1)
      '<' -> let l1 = advanceChar l
             in case peek l1 of
                  Just '-' -> Right (TBind, sp, advanceChar l1)
                  Just '=' -> Right (TLtEq, sp, advanceChar l1)
                  _        -> Right (TLt, sp, l1)
      '=' -> let l1 = advanceChar l
             in case peek l1 of
                  Just '=' -> Right (TEqEq, sp, advanceChar l1)
                  _        -> Right (TEquals, sp, l1)
      '>' -> let l1 = advanceChar l
             in case peek l1 of
                  Just '=' -> Right (TGtEq, sp, advanceChar l1)
                  _        -> Right (TGt, sp, l1)
      '!' -> let l1 = advanceChar l
             in case peek l1 of
                  Just '=' -> Right (TNeq, sp, advanceChar l1)
                  _        -> Left $ "Unexpected character '!' at " ++ show (spanLine sp) ++ ":" ++ show (spanCol sp)
      '"' -> lexString (advanceChar l) sp []
      _ | isDigit ch -> lexNumber l sp
        | isAlpha ch || ch == '_' -> lexIdent l sp
        | otherwise -> Left $ "Unexpected character: '" ++ [ch] ++ "'"

lexString :: Lexer -> Span -> String -> Either String (Token, Span, Lexer)
lexString l sp acc = case peek l of
  Nothing -> Left "Unterminated string literal"
  Just '"' -> Right (TStringLit (reverse acc), sp, advanceChar l)
  Just '\\' ->
    let l1 = advanceChar l
    in case peek l1 of
      Nothing -> Left "Unterminated string escape"
      Just 'n'  -> lexString (advanceChar l1) sp ('\n' : acc)
      Just 't'  -> lexString (advanceChar l1) sp ('\t' : acc)
      Just '\\' -> lexString (advanceChar l1) sp ('\\' : acc)
      Just '"'  -> lexString (advanceChar l1) sp ('"' : acc)
      Just c    -> lexString (advanceChar l1) sp (c : '\\' : acc)
  Just c -> lexString (advanceChar l) sp (c : acc)

lexNumber :: Lexer -> Span -> Either String (Token, Span, Lexer)
lexNumber l sp =
  let start = lexPos l
      l1 = consumeDigits l
      chars = lexChars l
  in case peek l1 of
    Just '.'
      | lexPos l1 + 1 < length chars && isDigit (chars !! (lexPos l1 + 1)) ->
        let l2 = consumeDigits (advanceChar l1)
            text = sliceChars l start (lexPos l2)
        in case reads text of
             [(f, "")] -> Right (TFloatLit f, sp, l2)
             _         -> Left $ "Invalid float: " ++ text
    _ ->
      let text = sliceChars l start (lexPos l1)
      in case reads text of
           [(n, "")] -> Right (TIntLit n, sp, l1)
           _         -> Left $ "Invalid integer: " ++ text
  where
    consumeDigits lx = case peek lx of
      Just c | isDigit c -> consumeDigits (advanceChar lx)
      _ -> lx

lexIdent :: Lexer -> Span -> Either String (Token, Span, Lexer)
lexIdent l sp =
  let start = lexPos l
      l1 = consumeIdent l
      word = sliceChars l start (lexPos l1)
      tok = case word of
        "type"      -> TType
        "alias"     -> TAlias
        "import"    -> TImport
        "dataframe" -> TDataframe
        "library"   -> TLibrary
        "package"   -> TPackage
        "exporting" -> TExporting
        "select"    -> TSelect
        "filter"    -> TFilter
        "mutate"    -> TMutate
        "summarize" -> TSummarize
        "group_by"  -> TGroupBy
        "arrange"   -> TArrange
        "let"       -> TLet
        "in"        -> TIn
        "case"      -> TCase
        "of"        -> TOf
        "if"        -> TIf
        "then"      -> TThen
        "else"      -> TElse
        "True"      -> TTrue
        "False"     -> TFalse
        "_"         -> TUnderscore
        _           -> TIdent word
  in Right (tok, sp, l1)
  where
    consumeIdent lx = case peek lx of
      Just c | isAlphaNum c || c == '_' -> consumeIdent (advanceChar lx)
      _ -> lx

tokenize :: String -> Either String ([Token], [Span])
tokenize input = go (newLexer input) [] []
  where
    go l toks sps = case nextToken l of
      Left err -> Left err
      Right (tok, sp, l') ->
        let toks' = toks ++ [tok]
            sps'  = sps ++ [sp]
        in if tok == TEof
           then Right (toks', sps')
           else go l' toks' sps'
