{-|
Module: Quone.Parse.Lexer

The tokenizer. Walks the source string from left to right and produces
a flat list of 'Token's together with a parallel list of 'Span's that
record where each token was found.

The lexer state is just a cursor: the remaining characters, the current
position, line, and column. We never look back.

If you've read a hand-written lexer in any language, this should look
familiar — there's a big @case@ on the next character and we recurse
to consume more.
-}
module Quone.Parse.Lexer
    ( tokenize
    ) where

import qualified Data.Char as Char

import Quone.Prelude
import Quone.Parse.Token


-- | The lexer's state as it walks the input.
data Lexer = Lexer
    { lexChars :: String
    , lexPos :: !Int
    , lexLine :: !Int
    , lexCol :: !Int
    }


newLexer :: String -> Lexer
newLexer input =
    Lexer input 0 1 1


-- | Look at the next character without consuming it.
peek :: Lexer -> Maybe Char
peek l =
    if lexPos l < length (lexChars l) then
        Just (lexChars l !! lexPos l)
    else
        Nothing


-- | Consume one character. Bumps the line/column counters appropriately.
advanceChar :: Lexer -> Lexer
advanceChar l
    | lexPos l >= length (lexChars l) =
        l

    | lexChars l !! lexPos l == '\n' =
        l { lexPos = lexPos l + 1, lexLine = lexLine l + 1, lexCol = 1 }

    | otherwise =
        l { lexPos = lexPos l + 1, lexCol = lexCol l + 1 }


-- | Skip past whitespace and @# ...@ comments.
skipWhitespaceAndComments :: Lexer -> Lexer
skipWhitespaceAndComments l =
    case peek l of
        Nothing ->
            l

        Just ch
            | Char.isSpace ch ->
                skipWhitespaceAndComments (advanceChar l)

            | ch == '#' ->
                skipWhitespaceAndComments (skipLine l)

            | otherwise ->
                l


skipLine :: Lexer -> Lexer
skipLine l =
    case peek l of
        Nothing ->
            l

        Just '\n' ->
            advanceChar l

        Just _ ->
            skipLine (advanceChar l)


currentSpan :: Lexer -> Span
currentSpan l =
    Span (lexLine l) (lexCol l)


-- | Slice characters from one position to another.
sliceChars :: Lexer -> Int -> Int -> String
sliceChars l start end =
    take (end - start) (drop start (lexChars l))


-- | Read the next token. Most of the work is dispatched by character.
nextToken :: Lexer -> Result String ( Token, Span, Lexer )
nextToken l0 =
    let
        l =
            skipWhitespaceAndComments l0

        sp =
            currentSpan l
    in
    case peek l of
        Nothing ->
            Ok ( TEof, sp, l )

        Just ch ->
            singleCharOrCompound ch sp l


singleCharOrCompound :: Char -> Span -> Lexer -> Result String ( Token, Span, Lexer )
singleCharOrCompound ch sp l =
    case ch of
        '(' ->
            Ok ( TLParen, sp, advanceChar l )

        ')' ->
            Ok ( TRParen, sp, advanceChar l )

        '{' ->
            Ok ( TLBrace, sp, advanceChar l )

        '}' ->
            Ok ( TRBrace, sp, advanceChar l )

        '[' ->
            Ok ( TLBracket, sp, advanceChar l )

        ']' ->
            Ok ( TRBracket, sp, advanceChar l )

        ',' ->
            Ok ( TComma, sp, advanceChar l )

        '.' ->
            let
                l1 =
                    advanceChar l
            in
            case peek l1 of
                Just '.' ->
                    Ok ( TDotDot, sp, advanceChar l1 )

                _ ->
                    Ok ( TDot, sp, l1 )

        ':' ->
            Ok ( TColon, sp, advanceChar l )

        '|' ->
            let
                l1 =
                    advanceChar l
            in
            case peek l1 of
                Just '>' ->
                    Ok ( TPipeRight, sp, advanceChar l1 )

                _ ->
                    Ok ( TPipe, sp, l1 )

        '+' ->
            Ok ( TPlus, sp, advanceChar l )

        '*' ->
            Ok ( TStar, sp, advanceChar l )

        '/' ->
            Ok ( TSlash, sp, advanceChar l )

        '\\' ->
            Ok ( TBackslash, sp, advanceChar l )

        '-' ->
            let
                l1 =
                    advanceChar l
            in
            case peek l1 of
                Just '>' ->
                    Ok ( TArrow, sp, advanceChar l1 )

                _ ->
                    Ok ( TMinus, sp, l1 )

        '<' ->
            let
                l1 =
                    advanceChar l
            in
            case peek l1 of
                Just '-' ->
                    Ok ( TBind, sp, advanceChar l1 )

                Just '=' ->
                    Ok ( TLtEq, sp, advanceChar l1 )

                _ ->
                    Ok ( TLt, sp, l1 )

        '=' ->
            let
                l1 =
                    advanceChar l
            in
            case peek l1 of
                Just '=' ->
                    Ok ( TEqEq, sp, advanceChar l1 )

                _ ->
                    Ok ( TEquals, sp, l1 )

        '>' ->
            let
                l1 =
                    advanceChar l
            in
            case peek l1 of
                Just '=' ->
                    Ok ( TGtEq, sp, advanceChar l1 )

                _ ->
                    Ok ( TGt, sp, l1 )

        '!' ->
            let
                l1 =
                    advanceChar l
            in
            case peek l1 of
                Just '=' ->
                    Ok ( TNeq, sp, advanceChar l1 )

                _ ->
                    Err
                        ( "Unexpected character '!' at "
                            ++ show (spanLine sp)
                            ++ ":"
                            ++ show (spanCol sp)
                        )

        '"' ->
            lexString (advanceChar l) sp []

        _
            | Char.isDigit ch ->
                lexNumber l sp

            | Char.isAlpha ch || ch == '_' ->
                lexIdent l sp

            | otherwise ->
                Err ("Unexpected character: '" ++ [ ch ] ++ "'")


-- | Consume a string literal, handling backslash escapes.
lexString :: Lexer -> Span -> String -> Result String ( Token, Span, Lexer )
lexString l sp acc =
    case peek l of
        Nothing ->
            Err "Unterminated string literal"

        Just '"' ->
            Ok ( TStringLit (reverse acc), sp, advanceChar l )

        Just '\\' ->
            let
                l1 =
                    advanceChar l
            in
            case peek l1 of
                Nothing ->
                    Err "Unterminated string escape"

                Just 'n' ->
                    lexString (advanceChar l1) sp ('\n' : acc)

                Just 't' ->
                    lexString (advanceChar l1) sp ('\t' : acc)

                Just '\\' ->
                    lexString (advanceChar l1) sp ('\\' : acc)

                Just '"' ->
                    lexString (advanceChar l1) sp ('"' : acc)

                Just c ->
                    lexString (advanceChar l1) sp (c : '\\' : acc)

        Just c ->
            lexString (advanceChar l) sp (c : acc)


-- | Consume a number. If we see a @.@ followed by a digit, it's a float;
-- otherwise it's an integer.
lexNumber :: Lexer -> Span -> Result String ( Token, Span, Lexer )
lexNumber l sp =
    let
        start =
            lexPos l

        l1 =
            consumeDigits l

        chars =
            lexChars l
    in
    case peek l1 of
        Just '.'
            | lexPos l1 + 1 < length chars && Char.isDigit (chars !! (lexPos l1 + 1)) ->
                let
                    l2 =
                        consumeDigits (advanceChar l1)

                    text =
                        sliceChars l start (lexPos l2)
                in
                case reads text of
                    [( f, "" )] ->
                        Ok ( TFloatLit f, sp, l2 )

                    _ ->
                        Err ("Invalid float: " ++ text)

        _ ->
            let
                text =
                    sliceChars l start (lexPos l1)
            in
            case reads text of
                [( n, "" )] ->
                    Ok ( TIntLit n, sp, l1 )

                _ ->
                    Err ("Invalid integer: " ++ text)


consumeDigits :: Lexer -> Lexer
consumeDigits l =
    case peek l of
        Just c | Char.isDigit c ->
            consumeDigits (advanceChar l)

        _ ->
            l


-- | Consume an identifier. Recognises keywords by looking the word up
-- in a small table.
lexIdent :: Lexer -> Span -> Result String ( Token, Span, Lexer )
lexIdent l sp =
    let
        start =
            lexPos l

        l1 =
            consumeIdent l

        word =
            sliceChars l start (lexPos l1)

        tok =
            keywordOrIdent word
    in
    Ok ( tok, sp, l1 )


consumeIdent :: Lexer -> Lexer
consumeIdent l =
    case peek l of
        Just c | Char.isAlphaNum c || c == '_' ->
            consumeIdent (advanceChar l)

        _ ->
            l


keywordOrIdent :: String -> Token
keywordOrIdent word =
    case word of
        "type" ->
            TType

        "alias" ->
            TAlias

        "import" ->
            TImport

        "dataframe" ->
            TDataframe

        "library" ->
            TLibrary

        "package" ->
            TPackage

        "exporting" ->
            TExporting

        "select" ->
            TSelect

        "filter" ->
            TFilter

        "mutate" ->
            TMutate

        "summarize" ->
            TSummarize

        "group_by" ->
            TGroupBy

        "arrange" ->
            TArrange

        "let" ->
            TLet

        "in" ->
            TIn

        "case" ->
            TCase

        "of" ->
            TOf

        "if" ->
            TIf

        "then" ->
            TThen

        "else" ->
            TElse

        "True" ->
            TTrue

        "False" ->
            TFalse

        "_" ->
            TUnderscore

        _ ->
            TIdent word


-- | Tokenize a whole source string. We collect tokens and spans into
-- two parallel lists, since that's what the parser expects.
tokenize :: String -> Result String ( List Token, List Span )
tokenize input =
    go (newLexer input) [] []
  where
    go l toks sps =
        case nextToken l of
            Err e ->
                Err e

            Ok ( tok, sp, l1 ) ->
                let
                    toks1 =
                        toks ++ [ tok ]

                    sps1 =
                        sps ++ [ sp ]
                in
                if tok == TEof then
                    Ok ( toks1, sps1 )
                else
                    go l1 toks1 sps1
