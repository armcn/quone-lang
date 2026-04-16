{-|
Module: Quone.Format

Pretty-print a parsed Quone program back into source code.

This is the formatter that powers @quone --format@ and the LSP's
"Format Document" command. It re-tokenizes and re-parses the source,
then walks the AST building a string with consistent indentation,
spacing, and line wrapping.

Why not edit the source text in place? Because formatting based on the
AST is far simpler: there's no whitespace or comment to track, just a
tree of values to print. The cost is that we drop everything we can't
recover from the AST — like comments inside expressions. We keep
header comments by detecting them at the start of the file and
re-inserting them at the top of the output.
-}
module Quone.Format
    ( formatSource
    ) where

import qualified Quone.List as List
import qualified Quone.Result as Result

import Quone.Prelude
import Quone.AST.Source
import Quone.Parse.Lexer (tokenize)
import Quone.Parse.Parser (parseProgram)



-- ====================================================================
-- ENTRY POINT
-- ====================================================================


-- | Re-parse a source file and pretty-print it. Returns 'Left' if the
-- source doesn't parse.
formatSource :: String -> Either String String
formatSource source = do
    ( toks, sps ) <- Result.toEither (tokenize source)
    prog <- parseProgram toks sps
    let
        headerComments =
            extractHeaderComments source

        body =
            fmtProgram prog

        out =
            if null headerComments then
                body
            else
                unlines headerComments ++ "\n" ++ body
    Right out


-- | Pull out top-of-file comments so they survive a format pass.
-- A comment "owns" the top of the file as long as we haven't seen any
-- non-comment, non-blank line yet.
extractHeaderComments :: String -> List String
extractHeaderComments source =
    go (lines source)
  where
    go [] =
        []

    go (l : ls)
        | isComment (strip l) =
            stripEnd l : go ls

        | null (strip l) =
            go ls

        | otherwise =
            []

    isComment ('#' : _) = True
    isComment _ = False

    strip =
        dropWhile (== ' ')

    stripEnd =
        reverse << dropWhile (== ' ') << reverse



-- ====================================================================
-- PROGRAM
-- ====================================================================


-- | A program is split into four sections, in this order:
--   1. The package declaration (if any)
--   2. @library ...@ declarations
--   3. @import ...@ declarations
--   4. Everything else (types, type aliases, value bindings)
-- Each section is separated by a blank line.
fmtProgram :: Program -> String
fmtProgram prog =
    let
        sections =
            List.concat
                [ moduleSection prog
                , librarySection prog
                , importSection prog
                , declSection prog
                ]

        body =
            List.intercalate "\n\n" sections
    in
    if null body || last body == '\n' then
        body
    else
        body ++ "\n"


moduleSection :: Program -> List String
moduleSection prog =
    case progModule prog of
        Just m ->
            [ fmtModuleDecl m ]

        Nothing ->
            []


librarySection :: Program -> List String
librarySection prog =
    let
        libs =
            [ n | DeclLibrary n <- progDecls prog ]
    in
    if null libs then
        []
    else
        [ unlines (List.map (\l -> "library " ++ l) libs) ]


importSection :: Program -> List String
importSection prog =
    let
        imps =
            [ e | DeclImport e <- progDecls prog ]
    in
    if null imps then
        []
    else
        [ unlines (List.map fmtImport imps) ]


declSection :: Program -> List String
declSection prog =
    [ fmtDecl d | d <- progDecls prog, isRestDecl d ]


isRestDecl :: Decl -> Bool
isRestDecl d =
    case d of
        DeclLibrary _ ->
            False

        DeclImport _ ->
            False

        _ ->
            True


fmtModuleDecl :: ModuleDecl -> String
fmtModuleDecl m =
    case moduleExports m of
        ExportAll ->
            "package " ++ moduleName m ++ " exporting (..)"

        ExportList ns ->
            "package "
                ++ moduleName m
                ++ " exporting ("
                ++ List.intercalate ", " ns
                ++ ")"


fmtImport :: ExternDecl -> String
fmtImport e =
    "import " ++ externName e ++ " : " ++ fmtTypeExprs (externTyExpr e)



-- ====================================================================
-- TYPE EXPRESSIONS
-- ====================================================================


fmtTypeExprs :: List TypeExpr -> String
fmtTypeExprs exprs =
    case exprs of
        [ TEFunc params ret ] ->
            List.intercalate " -> "
                (List.map fmtTypeExpr params ++ [ fmtTypeExpr ret ])

        [ single ] ->
            fmtTypeExpr single

        _ ->
            List.intercalate " -> " (List.map fmtTypeExpr exprs)


fmtTypeExpr :: TypeExpr -> String
fmtTypeExpr te =
    case te of
        TEVar v ->
            v

        TENamed name [] ->
            name

        TENamed name args ->
            let
                argStrs =
                    List.map
                        (\a ->
                            case a of
                                TENamed _ (_ : _) ->
                                    "(" ++ fmtTypeExpr a ++ ")"

                                _ ->
                                    fmtTypeExpr a
                        )
                        args
            in
            name ++ " " ++ unwords argStrs

        TEFunc params ret ->
            "("
                ++ List.intercalate " -> "
                    (List.map fmtTypeExpr params ++ [ fmtTypeExpr ret ])
                ++ ")"

        TERecord fields ->
            fmtRecordType fields

        TEDataframe fields ->
            fmtDataframeType fields


-- | Record types fit on a line when they have <=2 fields, otherwise
-- expand vertically with leading commas (Elm-style).
fmtRecordType :: List ( String, TypeExpr ) -> String
fmtRecordType fields
    | null fields =
        "{}"

    | length fields <= 2 =
        let
            inner =
                List.intercalate ", "
                    [ n ++ " : " ++ fmtTypeExpr t | ( n, t ) <- fields ]
        in
        "{ " ++ inner ++ " }"

    | otherwise =
        let
            ls =
                List.zipWith
                    (\i ( n, t ) ->
                        if i == (0 :: Int) then
                            "    { " ++ n ++ " : " ++ fmtTypeExpr t
                        else
                            "    , " ++ n ++ " : " ++ fmtTypeExpr t
                    )
                    [ 0 .. ]
                    fields
        in
        unlines (ls ++ [ "    }" ])


fmtDataframeType :: List ( String, TypeExpr ) -> String
fmtDataframeType fields
    | null fields =
        "dataframe {}"

    | length fields <= 2 =
        let
            inner =
                List.intercalate ", "
                    [ n ++ " : " ++ fmtTypeExpr t | ( n, t ) <- fields ]
        in
        "dataframe { " ++ inner ++ " }"

    | otherwise =
        let
            ls =
                "    dataframe"
                    : List.zipWith
                        (\i ( n, t ) ->
                            if i == (0 :: Int) then
                                "        { " ++ n ++ " : " ++ fmtTypeExpr t
                            else
                                "        , " ++ n ++ " : " ++ fmtTypeExpr t
                        )
                        [ 0 .. ]
                        fields
        in
        unlines (ls ++ [ "        }" ])



-- ====================================================================
-- DECLARATIONS
-- ====================================================================


fmtDecl :: Decl -> String
fmtDecl d =
    case d of
        DeclType td ->
            fmtTypeDecl td

        DeclTypeAlias ta ->
            fmtTypeAlias ta

        DeclLet ld ->
            fmtLetDecl ld

        DeclImport e ->
            fmtImport e

        DeclLibrary name ->
            "library " ++ name


fmtTypeDecl :: TypeDecl -> String
fmtTypeDecl td =
    let
        header =
            if null (typeParams td) then
                "type " ++ typeName td
            else
                "type " ++ typeName td ++ " " ++ unwords (typeParams td)

        ls =
            List.zipWith
                (\i v ->
                    let
                        fieldsStr =
                            if null (variantFields v) then
                                ""
                            else
                                " " ++ unwords (List.map fmtTypeExpr (variantFields v))
                    in
                    if i == (0 :: Int) then
                        header ++ "\n    <- " ++ variantName v ++ fieldsStr
                    else
                        "    | " ++ variantName v ++ fieldsStr
                )
                [ 0 .. ]
                (typeVariants td)
    in
    unlines ls


fmtTypeAlias :: TypeAliasDecl -> String
fmtTypeAlias ta =
    let
        header =
            if null (aliasParams ta) then
                "type alias " ++ aliasName ta ++ " <-"
            else
                "type alias "
                    ++ aliasName ta
                    ++ " "
                    ++ unwords (aliasParams ta)
                    ++ " <-"
    in
    case aliasBody ta of
        [ TERecord fields ] | length fields > 2 ->
            header ++ "\n" ++ fmtRecordType fields

        [ TEDataframe fields ] | length fields > 2 ->
            header ++ "\n" ++ fmtDataframeType fields

        _ ->
            header ++ " " ++ fmtTypeExprs (aliasBody ta)


fmtLetDecl :: LetDecl -> String
fmtLetDecl ld =
    let
        annStr =
            case letAnnotation ld of
                Just ann ->
                    letName ld ++ " : " ++ fmtTypeExprs ann ++ "\n"

                Nothing ->
                    ""
    in
    annStr ++ fmtLetBody ld


fmtLetBody :: LetDecl -> String
fmtLetBody ld =
    case letExpr ld of
        ELambda params body ->
            letName ld
                ++ " "
                ++ unwords params
                ++ " <-\n"
                ++ fmtBody body 4

        _ ->
            if isSimpleExpr (letExpr ld) then
                letName ld ++ " <-\n    " ++ fmtExprInline (letExpr ld)
            else
                letName ld ++ " <-\n" ++ fmtExprIndented (letExpr ld) 4



-- ====================================================================
-- EXPRESSION FORMATTING
-- ====================================================================


-- | True for "small" expressions that fit on a single inline line.
isSimpleExpr :: Expr -> Bool
isSimpleExpr e =
    case e of
        EVar{} -> True
        EInt{} -> True
        EFloat{} -> True
        EBool{} -> True
        EStr{} -> True
        EUnit -> True

        EVecLit elems ->
            length elems <= 5 && List.all isSimpleExpr elems

        ECall f args ->
            isSimpleExpr f
                && List.all isSimpleExpr args
                && length args <= 3

        ERecord fields ->
            length fields <= 3
                && List.all (isSimpleExpr << snd) fields

        EDataFrame fields ->
            length fields <= 3
                && List.all (isSimpleExpr << snd) fields

        EBinary _ l r ->
            isSimpleExpr l && isSimpleExpr r

        EFieldAccess e' _ ->
            isSimpleExpr e'

        _ ->
            False


-- | Format an expression that lives inside a function body.
-- This is the multi-line version: it knows how to lay out @let@,
-- @if@, @case@, and pipes across multiple lines.
fmtBody :: Expr -> Int -> String
fmtBody expr indent =
    let
        pad =
            replicate indent ' '
    in
    case expr of
        ELet bindings body ->
            pad
                ++ "let\n"
                ++ List.concatMap
                    (\( n, v ) -> fmtLetBinding n v (indent + 4) ++ "\n")
                    bindings
                ++ pad
                ++ "in\n"
                ++ fmtBody body indent

        EIf cond thenE elseE ->
            pad
                ++ "if "
                ++ fmtExprInline cond
                ++ " then\n"
                ++ fmtBody thenE (indent + 4)
                ++ "\n"
                ++ pad
                ++ "else\n"
                ++ fmtBody elseE (indent + 4)

        EMatch scrutinee arms ->
            pad
                ++ "case "
                ++ fmtExprInline scrutinee
                ++ " of\n"
                ++ List.concatMap (renderArm pad indent) (List.zip [ 0 .. ] arms)

        EPipe _ _ ->
            renderPipe expr indent pad

        _ ->
            pad ++ fmtExprInline expr


renderArm :: String -> Int -> ( Int, ( Pattern, Expr ) ) -> String
renderArm pad indent ( i, ( pat, body ) ) =
    let
        gap =
            if i > 0 then "\n" else ""
    in
    gap
        ++ pad
        ++ "    "
        ++ fmtPattern pat
        ++ " ->\n"
        ++ fmtBody body (indent + 8)


renderPipe :: Expr -> Int -> String -> String
renderPipe expr indent pad =
    let
        chain =
            collectPipeChain expr
    in
    case chain of
        [] ->
            pad

        first : rest ->
            fmtBody first indent
                ++ List.concatMap
                    (\step -> "\n" ++ pad ++ "|> " ++ fmtExprInline step)
                    rest


collectPipeChain :: Expr -> List Expr
collectPipeChain expr =
    case expr of
        EPipe left right ->
            collectPipeChain left ++ [ right ]

        _ ->
            [ expr ]


fmtLetBinding :: String -> Expr -> Int -> String
fmtLetBinding name val indent =
    let
        pad =
            replicate indent ' '
    in
    case val of
        ELambda params body | name /= "_" && '.' `notElem` name ->
            pad
                ++ name
                ++ " "
                ++ unwords params
                ++ " <-\n"
                ++ fmtBody body (indent + 4)

        _ ->
            pad ++ name ++ " <- " ++ fmtExprInline val


fmtExprIndented :: Expr -> Int -> String
fmtExprIndented e indent =
    case e of
        EPipe{} -> fmtBody e indent
        ELet{} -> fmtBody e indent
        EIf{} -> fmtBody e indent
        EMatch{} -> fmtBody e indent

        _ ->
            replicate indent ' ' ++ fmtExprInline e


-- | Format an expression as a single line.
fmtExprInline :: Expr -> String
fmtExprInline e =
    case e of
        EVar name -> name
        EInt n -> show n
        EFloat f -> let s = show f in if '.' `elem` s then s else s ++ ".0"
        EBool b -> if b then "True" else "False"
        EStr s -> "\"" ++ s ++ "\""
        EUnit -> "()"

        EVecLit elems ->
            "[" ++ List.intercalate ", " (List.map fmtExprInline elems) ++ "]"

        ELambda params body ->
            "(\\"
                ++ unwords params
                ++ " -> "
                ++ fmtExprInline body
                ++ ")"

        ECall func args ->
            fmtCall func args

        EIf cond thenE elseE ->
            "if "
                ++ fmtExprInline cond
                ++ " then "
                ++ fmtExprInline thenE
                ++ " else "
                ++ fmtExprInline elseE

        EBinary op l r ->
            fmtBinaryChild l op True
                ++ " "
                ++ binOpSymbol op
                ++ " "
                ++ fmtBinaryChild r op False

        ERecord fields ->
            fmtRecordLit fields

        EDataFrame fields ->
            fmtDataframeLit fields

        EFieldAccess e' field ->
            fmtExprInline e' ++ "." ++ field

        EPipe l r ->
            fmtExprInline l ++ " |> " ++ fmtExprInline r

        EDplyr verb args ->
            fmtDplyr verb args

        ELet{} -> fmtBody e 0
        EMatch{} -> fmtBody e 0


fmtCall :: Expr -> List Expr -> String
fmtCall func args
    | null args || (length args == 1 && isUnitE (head args)) =
        fmtExprInline func ++ " ()"

    | otherwise =
        let
            f =
                fmtExprInline func

            argStrs =
                List.map fmtCallArg (List.filter (not << isUnitE) args)
        in
        f ++ " " ++ unwords argStrs


isUnitE :: Expr -> Bool
isUnitE EUnit = True
isUnitE _ = False


-- | Decide whether a binary expression's child needs parentheses, based
-- on relative precedence.
fmtBinaryChild :: Expr -> BinOp -> Bool -> String
fmtBinaryChild e parentOp isLeft =
    case e of
        EBinary childOp _ _
            | (isLeft && binOpPrecedence childOp < binOpPrecedence parentOp)
                || (not isLeft && binOpPrecedence childOp <= binOpPrecedence parentOp) ->
                "(" ++ fmtExprInline e ++ ")"

        _ ->
            fmtExprInline e


fmtCallArg :: Expr -> String
fmtCallArg e =
    case e of
        ECall{} -> "(" ++ fmtExprInline e ++ ")"
        EBinary{} -> "(" ++ fmtExprInline e ++ ")"
        EIf{} -> "(" ++ fmtExprInline e ++ ")"
        _ -> fmtExprInline e


fmtRecordLit :: List ( String, Expr ) -> String
fmtRecordLit fields
    | length fields <= 3 && List.all (isSimpleExpr << snd) fields =
        let
            inner =
                List.intercalate ", "
                    [ n ++ " = " ++ fmtExprInline v | ( n, v ) <- fields ]
        in
        "{ " ++ inner ++ " }"

    | otherwise =
        let
            ls =
                List.zipWith
                    (\i ( n, v ) ->
                        if i == (0 :: Int) then
                            "    { " ++ n ++ " = " ++ fmtExprInline v
                        else
                            "    , " ++ n ++ " = " ++ fmtExprInline v
                    )
                    [ 0 .. ]
                    fields
        in
        unlines (ls ++ [ "    }" ])


fmtDataframeLit :: List ( String, Expr ) -> String
fmtDataframeLit fields
    | length fields <= 2 && List.all (isSimpleExpr << snd) fields =
        let
            inner =
                List.intercalate ", "
                    [ n ++ " = " ++ fmtExprInline v | ( n, v ) <- fields ]
        in
        "dataframe { " ++ inner ++ " }"

    | otherwise =
        let
            ls =
                "dataframe"
                    : List.zipWith
                        (\i ( n, v ) ->
                            if i == (0 :: Int) then
                                "        { " ++ n ++ " = " ++ fmtExprInline v
                            else
                                "        , " ++ n ++ " = " ++ fmtExprInline v
                        )
                        [ 0 .. ]
                        fields
        in
        unlines (ls ++ [ "        }" ])



-- ====================================================================
-- DPLYR
-- ====================================================================


fmtDplyr :: String -> List DplyrArg -> String
fmtDplyr verb args =
    case verb of
        "select" ->
            verb ++ " " ++ unwords [ n | DplyrColumn n <- args ]

        "group_by" ->
            verb ++ " " ++ unwords [ n | DplyrColumn n <- args ]

        "arrange" ->
            verb ++ " " ++ unwords (List.map fmtDplyrArg args)

        "filter" ->
            let
                preds =
                    [ fmtExprInline e | DplyrPred e <- args ]
            in
            verb ++ " " ++ unwords (List.map (\p -> "(" ++ p ++ ")") preds)

        "mutate" ->
            fmtDplyrNamed verb args

        "summarize" ->
            fmtDplyrNamed verb args

        _ ->
            verb


fmtDplyrNamed :: String -> List DplyrArg -> String
fmtDplyrNamed verb args =
    let
        entries =
            List.intercalate ", " (List.map fmtDplyrArg args)
    in
    verb ++ " { " ++ entries ++ " }"


fmtDplyrArg :: DplyrArg -> String
fmtDplyrArg arg =
    case arg of
        DplyrColumn name ->
            name

        DplyrPred expr ->
            "(" ++ fmtExprInline expr ++ ")"

        DplyrNamed name expr ->
            name ++ " = " ++ fmtExprInline expr



-- ====================================================================
-- PATTERNS
-- ====================================================================


fmtPattern :: Pattern -> String
fmtPattern pat =
    case pat of
        PWildcard ->
            "_"

        PVar name ->
            name

        PInt n ->
            show n

        PBool b ->
            if b then "True" else "False"

        PConstructor name fields
            | null fields ->
                name

            | otherwise ->
                name ++ " " ++ unwords (List.map fmtPattern fields)
