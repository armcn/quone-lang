{-|
Module: Quone

The public face of the Quone compiler.

Three things live here:

  1. The compiler pipeline ('compile', 'compileToR', 'check') that
     glues together lexing, parsing, type inference, and code generation.
  2. Hover and error-location helpers used by the LSP server to give
     the editor friendly types and underlines.
  3. The 'ReplState' type and 'replCompile' function that power the
     interactive REPL.

If you're trying to understand the codebase, start with 'compile' and
follow the calls down through the modules in @src/Quone/@.
-}
module Quone
    ( -- The compiler pipeline
      compile
    , compileToR
    , check
    , formatSource

      -- For the LSP
    , hoverInfo
    , HoverEntry(..)
    , CompileError(..)

      -- For package generation
    , generateNamespace
    , generateDescription
    , CompileResult(..)
    , ModuleInfo(..)

      -- For the REPL
    , ReplState(..)
    , newReplState
    , replCompile
    ) where

import qualified Quone.Dict as Dict
import qualified Quone.List as List
import qualified Quone.Parse.Lexer as Lexer
import qualified Quone.Result as Result
import qualified Quone.String as String
import qualified Quone.Type.Infer as Infer

import Quone.Prelude
import Quone.AST.Source
import Quone.Parse.Parser (parseProgram)
import Quone.Parse.Token (Token, Span)
import Quone.Type.Type (Ty(..), Scheme(..), assignVarNames, formatTyWith)
import Quone.Type.Infer (InferState(..))
import Quone.Generate.R (generate)
import qualified Quone.Format as Fmt



-- ====================================================================
-- PUBLIC TYPES
-- ====================================================================


-- | What 'compile' returns.
data CompileResult = CompileResult
    { crRCode :: String
    , crModule :: Maybe ModuleInfo
    }


-- | Information about a package declaration in the source file.
-- Used by 'generateNamespace' and 'generateDescription' when the user
-- asks for an R package.
data ModuleInfo = ModuleInfo
    { miName :: String
    , miExports :: List String
    }


-- | A type or parse error with a location.
data CompileError = CompileError
    { ceMessage :: String
    , ceLine :: Int
    , ceCol :: Int
    }
    deriving (Show)



-- ====================================================================
-- THE PIPELINE
-- ====================================================================


-- | Lex, parse, infer, and code-generate. Returns either a friendly
-- error string or a 'CompileResult' with the generated R.
compile :: String -> Either String CompileResult
compile source = do
    let
        fullSource =
            injectPrelude source
    ( toks, sps ) <- tokenize fullSource
    prog <- parseProgram toks sps
    ( types, inferSt ) <- inferProgram emptyInferState prog
    let
        rCode =
            generate prog types inferSt

        modInfo =
            fmap toModuleInfo (progModule prog)
    Right (CompileResult rCode modInfo)
  where
    toModuleInfo m =
        ModuleInfo
            { miName = moduleName m
            , miExports = resolveExports m
            }

    resolveExports m =
        case moduleExports m of
            ExportAll ->
                error "ExportAll requires program context"

            ExportList ns ->
                ns


-- | Like 'compile' but only returns the generated R code.
compileToR :: String -> Either String String
compileToR source =
    fmap crRCode (compile source)


-- | Run the pipeline up through type inference, returning any errors.
-- Used by the LSP to publish diagnostics.
check :: String -> List CompileError
check source =
    let
        isPackage =
            String.startsWith "package " (dropWhile (== ' ') source)

        preludeLineCount =
            length (lines preludeSource)

        lineOffset =
            if isPackage then 0 else preludeLineCount

        fullSource =
            injectPrelude source

        runPipeline = do
            ( toks, sps ) <- tokenize fullSource
            prog <- parseProgram toks sps
            _ <- inferProgram emptyInferState prog
            Right ()
    in
    case runPipeline of
        Right _ ->
            []

        Left msg ->
            [ buildErrorWithLocation msg source lineOffset ]


formatSource :: String -> Either String String
formatSource =
    Fmt.formatSource


emptyInferState :: InferState
emptyInferState =
    InferState 0 mempty [] mempty



-- ====================================================================
-- THE PRELUDE
-- ====================================================================


-- | The implicit prelude prepended to every Quone source file. These
-- are the @import@s that make basic R functions available.
preludeSource :: String
preludeSource =
    unlines
        [ "import sqrt : Double -> Double"
        , "import mean : Vector a -> Double"
        , "import sum : Vector a -> Double"
        , "import length : Vector a -> Integer"
        , "import to_double : Integer -> Double"
        ]


-- | Splice the prelude into the source. If the file starts with
-- @package@, we have to put the prelude /after/ that line so the
-- parser doesn't choke.
injectPrelude :: String -> String
injectPrelude source
    | String.startsWith "package " stripped =
        case findIndex '\n' source of
            Just pos ->
                take (pos + 1) source ++ preludeSource ++ drop (pos + 1) source

            Nothing ->
                source ++ "\n" ++ preludeSource

    | otherwise =
        preludeSource ++ source
  where
    stripped =
        dropWhile (== ' ') source


findIndex :: Eq a => a -> List a -> Maybe Int
findIndex needle =
    go 0
  where
    go _ [] =
        Nothing

    go i (x : xs)
        | x == needle =
            Just i

        | otherwise =
            go (i + 1) xs



-- ====================================================================
-- ERROR LOCATIONS
-- ====================================================================


-- | A compiler error from inference doesn't always have a useful line
-- and column. This function tries hard to find one by scanning the
-- source for clues based on what's mentioned in the error message.
buildErrorWithLocation :: String -> String -> Int -> CompileError
buildErrorWithLocation msg source lineOffset =
    case parseLocatedError msg of
        Just ( line, col, message ) ->
            let
                adjusted =
                    if line > lineOffset then line - lineOffset else 1
            in
            CompileError message adjusted col

        Nothing ->
            case findErrorLocation msg source of
                Just ( line, col ) ->
                    CompileError msg line col

                Nothing ->
                    CompileError msg 1 1


-- | Look for a leading "LINE:COL:..." in the error message.
parseLocatedError :: String -> Maybe ( Int, Int, String )
parseLocatedError msg =
    case break (== ':') msg of
        ( lineStr, ':' : rest ) ->
            case break (== ':') rest of
                ( colStr, ':' : message ) ->
                    case ( reads lineStr, reads colStr ) of
                        ( [( line, "" )], [( col, "" )] ) ->
                            Just ( line, col, message )

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


-- | Inference errors don't carry positions. Pull a quoted name out
-- of the message and search the source for it.
findErrorLocation :: String -> String -> Maybe ( Int, Int )
findErrorLocation msg source =
    let
        srcLines =
            List.zip [ 1 .. ] (lines source)

        innerMsg =
            stripDefPrefix msg

        specificPatterns =
            [ ( "Unknown column '", "'" )
            , ( "Unknown function or column '", "'" )
            , ( "Unbound variable: ", "" )
            , ( "Record has no field '", "'" )
            , ( "Cannot access field '", "'" )
            ]
    in
    case tryPatterns specificPatterns innerMsg srcLines of
        Just loc ->
            Just loc

        Nothing ->
            case tryPatterns specificPatterns msg srcLines of
                Just loc ->
                    Just loc

                Nothing ->
                    case extractName "In definition of '" "'" msg of
                        Just name ->
                            findDefBodyLocation name innerMsg srcLines

                        Nothing ->
                            tryKeywords msg srcLines


tryPatterns
    :: List ( String, String )
    -> String
    -> List ( Int, String )
    -> Maybe ( Int, Int )
tryPatterns [] _ _ =
    Nothing

tryPatterns (( prefix, suffix ) : rest) msg srcLines =
    case extractName prefix suffix msg of
        Just name ->
            case findNameInLines name srcLines of
                Just loc ->
                    Just loc

                Nothing ->
                    tryPatterns rest msg srcLines

        Nothing ->
            tryPatterns rest msg srcLines


tryKeywords :: String -> List ( Int, String ) -> Maybe ( Int, Int )
tryKeywords msg srcLines =
    let
        keywords =
            [ "mutate", "filter", "select", "summarize", "arrange", "group_by" ]

        go [] =
            Nothing

        go (kw : kws)
            | String.contains kw msg =
                findNameInLines kw srcLines

            | otherwise =
                go kws
    in
    go keywords


-- | Drop the "In definition of 'foo':" wrapper from an error so that
-- inner heuristics can see the underlying message.
stripDefPrefix :: String -> String
stripDefPrefix msg =
    let
        prefix =
            "In definition of '"
    in
    case findSubstring prefix msg of
        Nothing ->
            msg

        Just idx ->
            let
                afterPrefix =
                    drop (idx + length prefix) msg

                afterName =
                    drop 1 (dropWhile (/= '\'') afterPrefix)
            in
            case afterName of
                ':' : ' ' : rest -> rest
                ':' : rest -> rest
                _ -> msg


extractName :: String -> String -> String -> Maybe String
extractName prefix suffix msg =
    case findSubstring prefix msg of
        Nothing ->
            Nothing

        Just idx ->
            let
                rest =
                    drop (idx + length prefix) msg

                name =
                    if null suffix then
                        takeWhile (\c -> c /= ' ' && c /= '\n') rest
                    else
                        takeWhile (/= head suffix) rest
            in
            if null name then Nothing else Just name


findSubstring :: String -> String -> Maybe Int
findSubstring _ [] =
    Nothing

findSubstring needle haystack =
    go 0 haystack
  where
    go _ [] =
        Nothing

    go i s
        | String.startsWith needle s =
            Just i

        | otherwise =
            go (i + 1) (drop 1 s)


findNameInLines :: String -> List ( Int, String ) -> Maybe ( Int, Int )
findNameInLines _ [] =
    Nothing

findNameInLines name srcLines =
    case findDefLine name srcLines of
        Just loc ->
            Just loc

        Nothing ->
            findFirstOccurrence name srcLines


findDefLine :: String -> List ( Int, String ) -> Maybe ( Int, Int )
findDefLine name srcLines =
    case findBindBody srcLines of
        Just loc ->
            Just loc

        Nothing ->
            findWith (startsWithName name " :") srcLines
  where
    findBindBody [] =
        Nothing

    findBindBody (( lineNum, lineText ) : rest)
        | startsWithName name " <-" lineText =
            case rest of
                ( nextLine, _ ) : _ ->
                    Just ( nextLine, 1 )

                [] ->
                    Just ( lineNum, 1 )

        | otherwise =
            findBindBody rest

    findWith _ [] =
        Nothing

    findWith p (( lineNum, lineText ) : rest)
        | p lineText =
            Just ( lineNum, 1 )

        | otherwise =
            findWith p rest


findDefBodyLocation
    :: String
    -> String
    -> List ( Int, String )
    -> Maybe ( Int, Int )
findDefBodyLocation name innerMsg srcLines =
    let
        bodyLines =
            getBodyLines name srcLines

        keywords =
            extractMsgKeywords innerMsg
    in
    case List.concatMap (\kw -> findInLines kw bodyLines) keywords of
        loc : _ ->
            Just loc

        [] ->
            case bodyLines of
                ( ln, _ ) : _ ->
                    Just ( ln, 1 )

                [] ->
                    findNameInLines name srcLines


getBodyLines :: String -> List ( Int, String ) -> List ( Int, String )
getBodyLines name srcLines =
    case dropWhile (\( _, t ) -> not (startsWithName name " <-" t)) srcLines of
        _ : rest ->
            takeBody rest

        [] ->
            []
  where
    takeBody [] =
        []

    takeBody (( ln, txt ) : rest)
        | indented txt =
            ( ln, txt ) : takeBody rest

        | null (dropWhile (== ' ') txt) =
            takeBody rest

        | otherwise =
            []

    indented txt =
        case dropWhile (== ' ') txt of
            "" ->
                False

            '#' : _ ->
                False

            _ ->
                length (takeWhile (== ' ') txt) > 0


extractMsgKeywords :: String -> List String
extractMsgKeywords =
    extractQuoted


extractQuoted :: String -> List String
extractQuoted [] =
    []

extractQuoted ('\'' : rest) =
    let
        word =
            takeWhile (/= '\'') rest

        remaining =
            drop (length word + 1) rest
    in
    if null word then
        extractQuoted remaining
    else
        word : extractQuoted remaining

extractQuoted (_ : rest) =
    extractQuoted rest


findInLines :: String -> List ( Int, String ) -> List ( Int, Int )
findInLines _ [] =
    []

findInLines needle (( ln, txt ) : rest) =
    case findSubstring needle txt of
        Just col ->
            ( ln, col + 1 ) : findInLines needle rest

        Nothing ->
            findInLines needle rest


startsWithName :: String -> String -> String -> Bool
startsWithName name suffix lineText =
    let
        stripped =
            dropWhile (== ' ') lineText
    in
    String.startsWith (name ++ suffix) stripped


findFirstOccurrence :: String -> List ( Int, String ) -> Maybe ( Int, Int )
findFirstOccurrence _ [] =
    Nothing

findFirstOccurrence name (( lineNum, lineText ) : rest) =
    case findSubstring name lineText of
        Just col ->
            Just ( lineNum, col + 1 )

        Nothing ->
            findFirstOccurrence name rest



-- ====================================================================
-- HOVER (for the LSP)
-- ====================================================================


-- | One hover entry: the name of an identifier, its type, and where in
-- the source it's defined. Builtins get line/col 0 since they have no
-- source location.
data HoverEntry = HoverEntry
    { heName :: String
    , heTypeStr :: String
    , heLine :: Int
    , heCol :: Int
    }
    deriving (Show)


-- | Compute hover information for every binding in a source file plus
-- the standard builtins. Returns an empty list if the source doesn't
-- type-check.
hoverInfo :: String -> List HoverEntry
hoverInfo source =
    let
        isPackage =
            String.startsWith "package " (dropWhile (== ' ') source)

        preludeLineCount =
            length (lines preludeSource)

        lineOffset =
            if isPackage then 0 else preludeLineCount

        fullSource =
            injectPrelude source

        pipeline = do
            ( toks, sps ) <- tokenize fullSource
            prog <- parseProgram toks sps
            ( types, inferSt ) <- inferProgram emptyInferState prog
            Right ( types, inferSt, prog )
    in
    case pipeline of
        Left _ ->
            []

        Right ( types, inferSt, prog ) ->
            let
                fmtTy ty =
                    let
                        ( names, _ ) =
                            assignVarNames ty Dict.empty 0
                    in
                    formatTyWith names [] ty

                entries =
                    List.concatMap
                        (mkEntry fmtTy lineOffset source prog inferSt)
                        types

                builtinEntries =
                    mkBuiltinEntries fmtTy inferSt (List.map fst types)
            in
            entries ++ builtinEntries


mkEntry
    :: (Ty -> String)
    -> Int
    -> String
    -> Program
    -> InferState
    -> ( String, Ty )
    -> List HoverEntry
mkEntry fmtTy _lineOffset source prog inferSt ( name, ty ) =
    let
        tyStr =
            fmtTy ty

        ( line, col ) =
            findNameInSource name source

        entry =
            HoverEntry name tyStr line col

        paramEntries =
            case findLetDecl name prog of
                Just ld ->
                    mkParamEntries fmtTy source ld ty

                Nothing ->
                    []
    in
    entry : paramEntries


findLetDecl :: String -> Program -> Maybe LetDecl
findLetDecl name prog =
    List.find
        (\ld -> letName ld == name)
        [ ld | DeclLet ld <- progDecls prog ]


mkParamEntries
    :: (Ty -> String) -> String -> LetDecl -> Ty -> List HoverEntry
mkParamEntries fmtTy source ld ty =
    case ( letExpr ld, ty ) of
        ( ELambda params _, TyFunc paramTys _ ) ->
            [ HoverEntry p (fmtTy pt) line col
            | ( p, pt ) <- List.zip params paramTys
            , p /= "_"
            , let ( line, col ) = findNameInSource p source
            ]

        _ ->
            []


mkBuiltinEntries
    :: (Ty -> String) -> InferState -> List String -> List HoverEntry
mkBuiltinEntries fmtTy inferSt seen =
    let
        builtins =
            [ "map", "map2", "reduce", "keep", "discard" ]
    in
    List.concatMap (mkOneBuiltin fmtTy inferSt seen) builtins


mkOneBuiltin
    :: (Ty -> String)
    -> InferState
    -> List String
    -> String
    -> List HoverEntry
mkOneBuiltin fmtTy inferSt seen bname =
    if List.member bname seen then
        []
    else
        case List.find (\( n, _ ) -> n == bname) (env inferSt) of
            Just ( _, sc ) ->
                let
                    ty =
                        instantiateForDisplay inferSt sc

                    tyStr =
                        fmtTy ty
                in
                [ HoverEntry bname tyStr 0 0 ]

            Nothing ->
                []


instantiateForDisplay :: InferState -> Scheme -> Ty
instantiateForDisplay s sc =
    let
        mapping =
            Dict.fromList [ ( v, TyVar v ) | v <- schemeVars sc ]
    in
    applySubst s (substTy (schemeTy sc) mapping)


-- | Walk a type, applying a substitution dict.
substTy :: Ty -> Dict.Dict Int Ty -> Ty
substTy ty m =
    case ty of
        TyVar i ->
            case Dict.get i m of
                Just t -> t
                Nothing -> TyVar i

        TyInt -> TyInt
        TyDouble -> TyDouble
        TyBool -> TyBool
        TyStr -> TyStr

        TyFunc ps r ->
            TyFunc (List.map (\p -> substTy p m) ps) (substTy r m)

        TyAdt n args ->
            TyAdt n (List.map (\a -> substTy a m) args)

        TyRecord fs ->
            TyRecord (List.map (\( n, t ) -> ( n, substTy t m )) fs)


-- | Walk a type, applying the inference state's substitution.
applySubst :: InferState -> Ty -> Ty
applySubst s ty =
    case ty of
        TyVar i ->
            case Dict.get i (subst s) of
                Just resolved -> applySubst s resolved
                Nothing -> TyVar i

        TyInt -> TyInt
        TyDouble -> TyDouble
        TyBool -> TyBool
        TyStr -> TyStr

        TyFunc ps r ->
            TyFunc (List.map (applySubst s) ps) (applySubst s r)

        TyAdt n args ->
            TyAdt n (List.map (applySubst s) args)

        TyRecord fs ->
            TyRecord (List.map (\( n, t ) -> ( n, applySubst s t )) fs)


findNameInSource :: String -> String -> ( Int, Int )
findNameInSource name source =
    case findNameInLines name (List.zip [ 1 .. ] (lines source)) of
        Just ( l, c ) ->
            ( l, c )

        Nothing ->
            ( 1, 1 )



-- ====================================================================
-- R PACKAGE GENERATION
-- ====================================================================


-- | Build the contents of an R package's NAMESPACE file.
generateNamespace :: ModuleInfo -> String
generateNamespace info =
    List.concatMap (\n -> "export(" ++ n ++ ")\n") (miExports info)


-- | Build the contents of an R package's DESCRIPTION file.
generateDescription :: ModuleInfo -> String
generateDescription info =
    List.intercalate "\n"
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



-- ====================================================================
-- THE REPL
-- ====================================================================


-- | The REPL accumulates source and threads inference state across
-- entries. Each new line is appended to 'rsSource' so the type checker
-- sees the whole "script" so far.
data ReplState = ReplState
    { rsSource :: String
    , rsInferSt :: InferState
    , rsTypes :: List ( String, Ty )
    }


newReplState :: ReplState
newReplState =
    ReplState preludeSource emptyInferState []


-- | Compile one entry from the REPL. Returns the new R code to execute,
-- a string of type information to display, and the updated state.
--
-- We try the input as a declaration first; if that fails, we wrap it
-- as @it <- <expr>@ and try again as an expression.
replCompile
    :: ReplState
    -> String
    -> Either String ( String, String, ReplState )
replCompile rs input =
    case tryAsDecl rs input of
        Right result ->
            Right result

        Left _ ->
            case tryAsExpr rs input of
                Right result ->
                    Right result

                Left e ->
                    Left e


tryAsDecl :: ReplState -> String -> Either String ( String, String, ReplState )
tryAsDecl rs input =
    let
        fullSource =
            rsSource rs ++ "\n" ++ input
    in
    case replPipeline rs fullSource of
        Right ( rCode, types, inferSt ) ->
            let
                newTypes =
                    drop (length (rsTypes rs)) types
            in
            Right
                ( rCode
                , formatNewTypes newTypes
                , ReplState fullSource inferSt types
                )

        Left e ->
            Left e


tryAsExpr :: ReplState -> String -> Either String ( String, String, ReplState )
tryAsExpr rs input =
    let
        wrapped =
            "it <- " ++ input

        fullSource =
            rsSource rs ++ "\n" ++ wrapped
    in
    case replPipeline rs fullSource of
        Right ( rCode, _types, inferSt ) ->
            let
                printR =
                    rCode ++ "print(it)\n"
            in
            Right
                ( printR
                , ""
                , ReplState (rsSource rs) inferSt (rsTypes rs)
                )

        Left e ->
            Left e


-- | Compile a full source string and return only the new R code that
-- wasn't there last time, plus the types and inference state.
replPipeline
    :: ReplState
    -> String
    -> Either String ( String, List ( String, Ty ), InferState )
replPipeline rs src = do
    ( toks, sps ) <- tokenize src
    prog <- parseProgram toks sps
    ( types, inferSt ) <- inferProgram emptyInferState prog
    let
        rCode =
            generate prog types inferSt

        prevR =
            case compilePrev (rsSource rs) of
                Right r ->
                    r

                Left _ ->
                    ""

        newR =
            stripCommonPrefix rCode prevR
    Right ( newR, types, inferSt )


compilePrev :: String -> Either String String
compilePrev src = do
    ( toks, sps ) <- tokenize src
    prog <- parseProgram toks sps
    ( types, inferSt ) <- inferProgram emptyInferState prog
    Right (generate prog types inferSt)


-- | Drop the lines from the new R that match the start of the old R.
stripCommonPrefix :: String -> String -> String
stripCommonPrefix new old =
    let
        newLines =
            lines new

        oldLines =
            lines old
    in
    unlines (drop (length oldLines) newLines)


-- | Format a list of name/type pairs for display in the REPL.
formatNewTypes :: List ( String, Ty ) -> String
formatNewTypes [] =
    ""

formatNewTypes ts =
    let
        ( names, _ ) =
            List.foldl
                (\( _, ty ) ( m, c ) -> assignVarNames ty m c)
                ( Dict.empty, 0 )
                ts
    in
    List.concatMap
        (\( n, ty ) -> n ++ " : " ++ formatTyWith names [] ty ++ "\n")
        ts



-- ====================================================================
-- BOUNDARY HELPERS
-- ====================================================================


-- | Convert the lexer's 'Result' into a Haskell 'Either' so we can
-- use it inside @do@ blocks here. Once everything is on 'Result', we
-- can drop these.
tokenize :: String -> Either String ( List Token, List Span )
tokenize source =
    Result.toEither (Lexer.tokenize source)


inferProgram
    :: InferState
    -> Program
    -> Either String ( List ( String, Ty ), InferState )
inferProgram s p =
    Result.toEither (Infer.inferProgram s p)
