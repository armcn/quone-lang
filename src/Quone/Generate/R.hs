{-|
Module: Quone.Generate.R

Translate a typed Quone 'Program' into an R source file.

The generator walks the program declaration by declaration, building up
a string. We thread a small 'Codegen' state alongside it so we can:

  - look up constructor info when generating pattern matches
  - hand out fresh temporary variable names for @case@ scrutinees

The high-level shape of the output is:

> library(...)
>
> .make_variant <- function(...) { ... }   -- only if the program has ADTs
>
> Constructor1 <- function(...) { ... }    -- one per ADT variant
> Constructor2 <- ...
>
> #' @param x [Type]                       -- roxygen for top-level functions
> name <- function(x) { ... }

There are a couple of optimisations:

  - Simple lambdas over vectors get fused into vectorised R expressions
    instead of @purrr::map_dbl@ (see 'genVectorized').
  - Pipe chains are emitted directly as @|>@ pipelines instead of nested
    function calls (see 'genPipeRhs').
-}
module Quone.Generate.R
    ( generate
    ) where

import qualified Quone.Dict as Dict
import qualified Quone.List as List
import Quone.Dict (Dict)

import Quone.Prelude
import Quone.AST.Source
import Quone.Type.Type
import Quone.Type.Infer (InferState(..))



-- ====================================================================
-- STATE
-- ====================================================================


-- | The state threaded through code generation. Most generation
-- functions take a 'Codegen' and return a new 'Codegen' alongside their
-- output string.
data Codegen = Codegen
    { cgConstructors :: Dict String CtorInfo
    , cgTmpCounter :: !Int
    , cgAliasMap :: List ( String, List ( String, Ty ) )
    }


-- | What we remember about a custom-type constructor.
data CtorInfo = CtorInfo
    { _ciTypeName :: String
    , ciArity :: Int
    }


newCodegen :: Program -> InferState -> Codegen
newCodegen prog _inferSt =
    let
        ctors =
            Dict.fromList
                [ ( variantName v, CtorInfo (typeName td) (length (variantFields v)) )
                | DeclType td <- progDecls prog
                , v <- typeVariants td
                ]
    in
    Codegen ctors 0 []



-- ====================================================================
-- ENTRY POINT
-- ====================================================================


-- | Translate a whole program into R source code.
generate :: Program -> List ( String, Ty ) -> InferState -> String
generate prog types inferSt =
    let
        cg0 =
            newCodegen prog inferSt

        typeMap =
            Dict.fromList types
    in
    runGen cg0 prog typeMap


runGen :: Codegen -> Program -> Dict String Ty -> String
runGen cg0 prog typeMap =
    let
        libs =
            List.concatMap genLibrary (progDecls prog)

        libSep =
            if List.any isLib (progDecls prog) then "\n" else ""

        variant =
            if hasTypeDecls prog then
                makeVariant
                    ++ "\n"
                    ++ List.concatMap (genTypeDecl cg0) (progDecls prog)
            else
                ""

        addDecl ( acc, cg ) d =
            case d of
                DeclLet ld ->
                    let
                        roxy =
                            case Dict.get (letName ld) typeMap of
                                Just ty ->
                                    genRoxygen ld ty (cgAliasMap cg)

                                Nothing ->
                                    ""

                        export =
                            if isExported prog (letName ld) then
                                "#' @export\n"
                            else
                                ""

                        ( code, cg1 ) =
                            genLetDecl cg ld
                    in
                    ( acc ++ roxy ++ export ++ code ++ "\n", cg1 )

                _ ->
                    ( acc, cg )

        ( defs, _ ) =
            List.foldl (flip addDecl) ( "", cg0 ) (progDecls prog)
    in
    libs ++ libSep ++ variant ++ defs


hasTypeDecls :: Program -> Bool
hasTypeDecls prog =
    List.any isTypeDecl (progDecls prog)
  where
    isTypeDecl (DeclType _) = True
    isTypeDecl _ = False


isLib :: Decl -> Bool
isLib (DeclLibrary _) = True
isLib _ = False


isExported :: Program -> String -> Bool
isExported prog name =
    case progModule prog of
        Nothing ->
            False

        Just m ->
            case moduleExports m of
                ExportAll ->
                    True

                ExportList ns ->
                    List.member name ns



-- ====================================================================
-- TOP-LEVEL DECLARATIONS
-- ====================================================================


genLibrary :: Decl -> String
genLibrary d =
    case d of
        DeclLibrary name ->
            "library(" ++ name ++ ")\n"

        _ ->
            ""


genTypeDecl :: Codegen -> Decl -> String
genTypeDecl cg d =
    case d of
        DeclType td ->
            List.concatMap (genConstructor cg (typeName td)) (typeVariants td)

        _ ->
            ""


-- | Emit one constructor function. Nullary constructors are functions
-- of zero arguments; constructors with fields take one argument per field.
genConstructor :: Codegen -> String -> VariantDecl -> String
genConstructor _ typN v
    | null (variantFields v) =
        variantName v
            ++ " <- function() {\n  .make_variant('"
            ++ typN
            ++ "', '"
            ++ variantName v
            ++ "')\n}\n\n"

    | otherwise =
        let
            n =
                length (variantFields v)

            params =
                [ "field" ++ show i | i <- [ 1 .. n ] ]

            paramsStr =
                List.intercalate ", " params

            fieldsStr =
                List.intercalate ", " (List.map (\p -> p ++ " = " ++ p) params)
        in
        variantName v
            ++ " <- function("
            ++ paramsStr
            ++ ") {\n  .make_variant('"
            ++ typN
            ++ "', '"
            ++ variantName v
            ++ "', list("
            ++ fieldsStr
            ++ "))\n}\n\n"


-- | Generate roxygen comments for a top-level function. We only emit
-- @\@param@ and @\@return@ for actual functions; bare values get nothing.
genRoxygen :: LetDecl -> Ty -> List ( String, List ( String, Ty ) ) -> String
genRoxygen ld ty aliases =
    let
        ( names, _ ) =
            assignVarNames ty Dict.empty 0
    in
    case ( letExpr ld, ty ) of
        ( ELambda params _, TyFunc paramTys retTy ) ->
            let
                paramLines =
                    List.zipWith
                        (\name pty ->
                            "#' @param "
                                ++ name
                                ++ " ["
                                ++ formatTyWith names aliases pty
                                ++ "]"
                        )
                        params
                        paramTys

                retLine =
                    "#' @return [" ++ formatTyWith names aliases retTy ++ "]"
            in
            unlines (paramLines ++ [ retLine ])

        _ ->
            ""


-- | Generate the binding for a single 'LetDecl'. Functions become
-- @function() { ... }@; everything else is a plain @<-@ assignment.
genLetDecl :: Codegen -> LetDecl -> ( String, Codegen )
genLetDecl cg ld =
    case letExpr ld of
        ELambda params body ->
            let
                ( bodyStr, cg1 ) =
                    genBody cg body
            in
            ( letName ld
                ++ " <- function("
                ++ List.intercalate ", " params
                ++ ") {\n"
                ++ indentLines 2 bodyStr
                ++ "\n}\n"
            , cg1
            )

        _ ->
            let
                ( exprStr, cg1 ) =
                    genExpr cg (letExpr ld)
            in
            ( letName ld ++ " <- " ++ exprStr ++ "\n", cg1 )



-- ====================================================================
-- BODIES
-- ====================================================================


-- | A function body. The difference from 'genExpr' is that 'ELet'
-- bindings at the top of a body are emitted as plain R statements
-- rather than wrapped in a @local({ ... })@.
genBody :: Codegen -> Expr -> ( String, Codegen )
genBody cg expr =
    case expr of
        ELet bindings body ->
            let
                emitBinding ( acc, cg' ) ( name, e ) =
                    if name == "_" then
                        let
                            ( es, cg'' ) =
                                genExpr cg' e
                        in
                        ( acc ++ [ es ], cg'' )
                    else
                        case splitDotted name of
                            Just ( obj, field ) ->
                                let
                                    ( es, cg'' ) =
                                        genExpr cg' e
                                in
                                ( acc ++ [ obj ++ "$" ++ field ++ " <- " ++ es ]
                                , cg''
                                )

                            Nothing ->
                                emitNamedBinding cg' acc name e

                ( bindLines, cg1 ) =
                    List.foldl (flip emitBinding) ( [], cg ) bindings

                ( bodyStr, cg2 ) =
                    genBody cg1 body
            in
            ( unlines bindLines ++ bodyStr, cg2 )

        _ ->
            genExpr cg expr


emitNamedBinding
    :: Codegen
    -> List String
    -> String
    -> Expr
    -> ( List String, Codegen )
emitNamedBinding cg' acc name e =
    case e of
        ELambda params lbody ->
            let
                ( bs, cg'' ) =
                    genBody cg' lbody
            in
            ( acc
                ++ [ name
                        ++ " <- function("
                        ++ List.intercalate ", " params
                        ++ ") {\n"
                        ++ indentLines 2 bs
                        ++ "\n}"
                   ]
            , cg''
            )

        _ ->
            let
                ( es, cg'' ) =
                    genExpr cg' e
            in
            ( acc ++ [ name ++ " <- " ++ es ], cg'' )


splitDotted :: String -> Maybe ( String, String )
splitDotted s =
    case break (== '.') s of
        ( _, "" ) ->
            Nothing

        ( a, _ : b ) ->
            Just ( a, b )



-- ====================================================================
-- EXPRESSIONS
-- ====================================================================


-- | Translate one expression into R syntax, returning the rendered
-- string and the updated 'Codegen' state.
genExpr :: Codegen -> Expr -> ( String, Codegen )
genExpr cg expr =
    case expr of
        EVar name ->
            case Dict.get name (cgConstructors cg) of
                Just info | ciArity info == 0 ->
                    ( name ++ "()", cg )

                _ ->
                    ( name, cg )

        EInt n ->
            ( show n ++ "L", cg )

        EFloat f ->
            ( showDouble f, cg )

        EBool b ->
            ( if b then "TRUE" else "FALSE", cg )

        EStr s ->
            ( "\"" ++ List.concatMap escChar s ++ "\"", cg )

        EVecLit elems ->
            let
                ( parts, cg1 ) =
                    mapAccumCg cg elems genExpr
            in
            ( "c(" ++ List.intercalate ", " parts ++ ")", cg1 )

        ELambda params body ->
            let
                ( bodyStr, cg1 ) =
                    genBody cg body
            in
            ( "function("
                ++ List.intercalate ", " params
                ++ ") {\n"
                ++ indentLines 2 bodyStr
                ++ "\n}"
            , cg1
            )

        EUnit ->
            ( "", cg )

        ECall func args ->
            genCall cg func args

        EIf cond thenE elseE ->
            genIf cg cond thenE elseE

        EMatch scrutinee arms ->
            genMatch cg scrutinee arms

        EBinary op l r ->
            genBinary cg op l r

        ELet bindings body ->
            genLetBlock cg bindings body

        ERecord fields ->
            genListLiteral cg "list" fields

        EDataFrame fields ->
            genDataFrame cg fields

        EFieldAccess inner field ->
            let
                ( es, cg1 ) =
                    genExpr cg inner
            in
            ( es ++ "$" ++ field, cg1 )

        EDplyr verb args ->
            genDplyr cg verb args

        EPipe _ _ ->
            genPipeChain cg expr


-- | Show a 'Double' the way R wants it: always with a decimal point.
showDouble :: Double -> String
showDouble f =
    let
        s =
            show f
    in
    if '.' `elem` s then s else s ++ ".0"


genCall :: Codegen -> Expr -> List Expr -> ( String, Codegen )
genCall cg func args =
    case func of
        EVar name | Just code <- genBuiltinCall cg name args ->
            code

        _ ->
            let
                realArgs =
                    List.filter (not << isUnitExpr) args

                ( argStrs, cg1 ) =
                    mapAccumCg cg realArgs genExpr

                ( funcStr, cg2 ) =
                    case func of
                        EVar name ->
                            ( name, cg1 )

                        _ ->
                            genExpr cg1 func
            in
            ( funcStr ++ "(" ++ List.intercalate ", " argStrs ++ ")", cg2 )


genIf :: Codegen -> Expr -> Expr -> Expr -> ( String, Codegen )
genIf cg cond thenE elseE =
    let
        ( c, cg1 ) =
            genExpr cg cond

        ( t, cg2 ) =
            genExpr cg1 thenE

        ( e, cg3 ) =
            genExpr cg2 elseE
    in
    ( "if ("
        ++ c
        ++ ") {\n"
        ++ indentLines 2 t
        ++ "\n} else {\n"
        ++ indentLines 2 e
        ++ "\n}"
    , cg3
    )


genBinary :: Codegen -> BinOp -> Expr -> Expr -> ( String, Codegen )
genBinary cg op left right =
    let
        prec =
            binOpPrecedence op

        ( l, cg1 ) =
            genExpr cg left

        lStr =
            case left of
                EBinary lop _ _ | binOpPrecedence lop < prec ->
                    "(" ++ l ++ ")"

                _ ->
                    l

        ( r, cg2 ) =
            genExpr cg1 right

        rStr =
            case right of
                EBinary rop _ _ | binOpPrecedence rop <= prec ->
                    "(" ++ r ++ ")"

                _ ->
                    r
    in
    ( lStr ++ " " ++ binOpSymbol op ++ " " ++ rStr, cg2 )


genLetBlock
    :: Codegen
    -> List ( String, Expr )
    -> Expr
    -> ( String, Codegen )
genLetBlock cg bindings body =
    let
        emit ( acc, cg' ) ( name, e ) =
            case e of
                ELambda params lbody ->
                    let
                        ( bs, cg'' ) =
                            genBody cg' lbody
                    in
                    ( acc
                        ++ [ name
                                ++ " <- function("
                                ++ List.intercalate ", " params
                                ++ ") {\n"
                                ++ indentLines 2 bs
                                ++ "\n}"
                           ]
                    , cg''
                    )

                _ ->
                    let
                        ( es, cg'' ) =
                            genExpr cg' e
                    in
                    ( acc ++ [ name ++ " <- " ++ es ], cg'' )

        ( bindParts, cg1 ) =
            List.foldl (flip emit) ( [], cg ) bindings

        ( bodyStr, cg2 ) =
            genBody cg1 body

        inner =
            unlines bindParts ++ bodyStr
    in
    ( "local({\n" ++ indentLines 2 inner ++ "\n})", cg2 )


genListLiteral
    :: Codegen
    -> String
    -> List ( String, Expr )
    -> ( String, Codegen )
genListLiteral cg fnName fields =
    let
        emit cg' ( n, e ) =
            let
                ( es, cg'' ) =
                    genExpr cg' e
            in
            ( n ++ " = " ++ es, cg'' )

        ( parts, cg1 ) =
            mapAccumCg cg fields emit
    in
    ( fnName ++ "(" ++ List.intercalate ", " parts ++ ")", cg1 )


-- | Like 'genListLiteral' but breaks across multiple lines if the
-- single-line version is too wide.
genDataFrame
    :: Codegen
    -> List ( String, Expr )
    -> ( String, Codegen )
genDataFrame cg fields =
    let
        emit cg' ( n, e ) =
            let
                ( es, cg'' ) =
                    genExpr cg' e
            in
            ( n ++ " = " ++ es, cg'' )

        ( parts, cg1 ) =
            mapAccumCg cg fields emit

        oneLine =
            "data.frame(" ++ List.intercalate ", " parts ++ ")"
    in
    if length oneLine > 80 then
        ( "data.frame(\n"
            ++ List.intercalate ",\n" (List.map (\p -> "  " ++ p) parts)
            ++ "\n)"
        , cg1
        )
    else
        ( oneLine, cg1 )


genDplyr :: Codegen -> String -> List DplyrArg -> ( String, Codegen )
genDplyr cg verb args =
    let
        emit cg' a =
            case a of
                DplyrColumn name ->
                    ( name, cg' )

                DplyrPred predicate ->
                    genColExpr cg' predicate

                DplyrNamed name predicate ->
                    let
                        ( es, cg'' ) =
                            genColExpr cg' predicate
                    in
                    ( name ++ " = " ++ es, cg'' )

        ( parts, cg1 ) =
            mapAccumCg cg args emit
    in
    ( "dplyr::" ++ verb ++ "(" ++ List.intercalate ", " parts ++ ")", cg1 )



-- ====================================================================
-- PIPES
-- ====================================================================


-- | Turn a chain of 'EPipe's into an R native pipe (@|>@) chain.
genPipeChain :: Codegen -> Expr -> ( String, Codegen )
genPipeChain cg expr =
    let
        ( steps, base ) =
            collectPipeSteps expr

        ( first, cg1 ) =
            genExpr cg base

        firstStr =
            if needsPipeParens base then
                "(" ++ first ++ ")"
            else
                first

        emit ( acc, cg' ) step =
            let
                ( rhs, cg'' ) =
                    genPipeRhs cg' step
            in
            ( acc ++ " |>\n  " ++ rhs, cg'' )

        ( result, cg2 ) =
            List.foldl (flip emit) ( firstStr, cg1 ) steps
    in
    ( result, cg2 )


-- | Walk left-leaning 'EPipe' structure and return each step in order
-- together with the base expression at the bottom.
collectPipeSteps :: Expr -> ( List Expr, Expr )
collectPipeSteps expr =
    case expr of
        EPipe left right ->
            let
                ( steps, base ) =
                    collectPipeSteps left
            in
            ( steps ++ [ right ], base )

        _ ->
            ( [], expr )


needsPipeParens :: Expr -> Bool
needsPipeParens expr =
    case expr of
        EBinary{} ->
            True

        ECall (EVar name) args
            | name == "map" && length args == 2 ->
                isVecBody args

            | name == "map2" && length args == 3 ->
                isVecBody args

            | otherwise ->
                False

        _ ->
            False


isVecBody :: List Expr -> Bool
isVecBody args =
    case args of
        ELambda params body : _ ->
            length params <= 2 && isVectorizableBody body

        _ ->
            False


-- | Generate the right-hand side of a pipe step. There are special cases
-- for builtins that can be vectorized.
genPipeRhs :: Codegen -> Expr -> ( String, Codegen )
genPipeRhs cg expr =
    case expr of
        EDplyr{} ->
            genExpr cg expr

        EVar name ->
            ( builtinRName name ++ "()", cg )

        ECall (EVar name) args
            | Just ( code, cg1 ) <- genBuiltinPipeRhs cg name args ->
                ( code, cg1 )

        ECall func args ->
            let
                ( argStrs, cg1 ) =
                    mapAccumCg cg args genExpr

                ( funcStr, cg2 ) =
                    case func of
                        EVar name ->
                            ( name, cg1 )

                        _ ->
                            genExpr cg1 func
            in
            ( funcStr ++ "(" ++ List.intercalate ", " argStrs ++ ")", cg2 )

        _ ->
            let
                ( code, cg1 ) =
                    genExpr cg expr
            in
            ( "(" ++ code ++ ")", cg1 )



-- ====================================================================
-- BUILTINS
-- ====================================================================


-- | Translate a call to a known builtin. Returns 'Nothing' if @name@
-- isn't a builtin.
genBuiltinCall :: Codegen -> String -> List Expr -> Maybe ( String, Codegen )
genBuiltinCall cg name args =
    case ( name, args ) of
        ( "map", [ ELambda [ p ] body, xs ] ) | isVectorizableBody body ->
            let
                ( xsStr, cg1 ) =
                    genExpr cg xs

                subs =
                    Dict.singleton p xsStr

                ( result, cg2 ) =
                    genVectorized cg1 body subs
            in
            Just ( result, cg2 )

        ( "map", [ f, xs ] ) ->
            let
                ( fStr, cg1 ) =
                    genExpr cg f

                ( xsStr, cg2 ) =
                    genExpr cg1 xs
            in
            Just ( "purrr::map_dbl(" ++ xsStr ++ ", " ++ fStr ++ ")", cg2 )

        ( "map2", [ ELambda [ p1, p2 ] body, xs, ys ] ) | isVectorizableBody body ->
            let
                ( xsStr, cg1 ) =
                    genExpr cg xs

                ( ysStr, cg2 ) =
                    genExpr cg1 ys

                subs =
                    Dict.fromList [ ( p1, xsStr ), ( p2, ysStr ) ]

                ( result, cg3 ) =
                    genVectorized cg2 body subs
            in
            Just ( result, cg3 )

        ( "map2", [ f, xs, ys ] ) ->
            let
                ( fStr, cg1 ) =
                    genExpr cg f

                ( xsStr, cg2 ) =
                    genExpr cg1 xs

                ( ysStr, cg3 ) =
                    genExpr cg2 ys
            in
            Just
                ( "purrr::map2_dbl("
                    ++ xsStr
                    ++ ", "
                    ++ ysStr
                    ++ ", "
                    ++ fStr
                    ++ ")"
                , cg3
                )

        ( "reduce", [ f, ini, xs ] ) ->
            let
                ( fStr, cg1 ) = genExpr cg f
                ( iniStr, cg2 ) = genExpr cg1 ini
                ( xsStr, cg3 ) = genExpr cg2 xs
            in
            Just
                ( "purrr::reduce("
                    ++ xsStr
                    ++ ", "
                    ++ fStr
                    ++ ", .init = "
                    ++ iniStr
                    ++ ")"
                , cg3
                )

        ( "keep", [ f, xs ] ) ->
            let
                ( fStr, cg1 ) = genExpr cg f
                ( xsStr, cg2 ) = genExpr cg1 xs
            in
            Just ( "purrr::keep(" ++ xsStr ++ ", " ++ fStr ++ ")", cg2 )

        ( "discard", [ f, xs ] ) ->
            let
                ( fStr, cg1 ) = genExpr cg f
                ( xsStr, cg2 ) = genExpr cg1 xs
            in
            Just ( "purrr::discard(" ++ xsStr ++ ", " ++ fStr ++ ")", cg2 )

        ( "to_double", [ x ] ) ->
            let
                ( xStr, cg1 ) = genExpr cg x
            in
            Just ( "as.numeric(" ++ xStr ++ ")", cg1 )

        _ ->
            Nothing


-- | Same as 'genBuiltinCall' but for the case where the builtin is on
-- the right side of a pipe (so the data argument is missing).
genBuiltinPipeRhs :: Codegen -> String -> List Expr -> Maybe ( String, Codegen )
genBuiltinPipeRhs cg name args =
    case ( name, args ) of
        ( "map", [ ELambda [ p ] body ] ) | isVectorizableBody body ->
            let
                subs =
                    Dict.singleton p ".x"

                ( result, cg1 ) =
                    genVectorized cg body subs
            in
            Just ( "(\\(.x) " ++ result ++ ")()", cg1 )

        ( "map", [ f ] ) ->
            let
                ( fStr, cg1 ) = genExpr cg f
            in
            Just ( "purrr::map_dbl(" ++ fStr ++ ")", cg1 )

        ( "map2", [ ELambda [ p1, p2 ] body, ys ] ) | isVectorizableBody body ->
            let
                ( ysStr, cg1 ) = genExpr cg ys
                subs = Dict.fromList [ ( p1, ".x" ), ( p2, ysStr ) ]
                ( result, cg2 ) = genVectorized cg1 body subs
            in
            Just ( "(\\(.x) " ++ result ++ ")()", cg2 )

        ( "map2", [ f, ys ] ) ->
            let
                ( fStr, cg1 ) = genExpr cg f
                ( ysStr, cg2 ) = genExpr cg1 ys
            in
            Just ( "purrr::map2_dbl(" ++ ysStr ++ ", " ++ fStr ++ ")", cg2 )

        ( "reduce", [ f, ini ] ) ->
            let
                ( fStr, cg1 ) = genExpr cg f
                ( iniStr, cg2 ) = genExpr cg1 ini
            in
            Just ( "purrr::reduce(" ++ fStr ++ ", .init = " ++ iniStr ++ ")", cg2 )

        ( "keep", [ f ] ) ->
            let
                ( fStr, cg1 ) = genExpr cg f
            in
            Just ( "purrr::keep(" ++ fStr ++ ")", cg1 )

        ( "discard", [ f ] ) ->
            let
                ( fStr, cg1 ) = genExpr cg f
            in
            Just ( "purrr::discard(" ++ fStr ++ ")", cg1 )

        _ ->
            Nothing


-- | Is an expression simple enough to fuse into vectorised R code?
-- Vectorisable expressions can be lifted out of @purrr::map@ since R's
-- arithmetic and a handful of unary functions already work elementwise.
isVectorizableBody :: Expr -> Bool
isVectorizableBody expr =
    case expr of
        EVar{} -> True
        EInt{} -> True
        EFloat{} -> True
        EBool{} -> True

        EBinary _ l r ->
            isVectorizableBody l && isVectorizableBody r

        ECall (EVar name) [ arg ] ->
            List.member name [ "sqrt", "abs", "log", "exp" ]
                && isVectorizableBody arg

        _ ->
            False


-- | Produce a vectorised R expression from a Quone body, replacing each
-- variable that appears in the substitution with the corresponding R
-- expression.
genVectorized
    :: Codegen
    -> Expr
    -> Dict String String
    -> ( String, Codegen )
genVectorized cg body subs =
    case body of
        EVar name ->
            ( case Dict.get name subs of
                Just s ->
                    s

                Nothing ->
                    name
            , cg
            )

        EInt n ->
            ( show n ++ "L", cg )

        EFloat f ->
            ( showDouble f, cg )

        EBool b ->
            ( if b then "TRUE" else "FALSE", cg )

        EBinary op l r ->
            let
                prec =
                    binOpPrecedence op

                ( lStr, cg1 ) =
                    genVectorized cg l subs

                lFinal =
                    case l of
                        EBinary lop _ _ | binOpPrecedence lop < prec ->
                            "(" ++ lStr ++ ")"

                        _ ->
                            lStr

                ( rStr, cg2 ) =
                    genVectorized cg1 r subs

                rFinal =
                    case r of
                        EBinary rop _ _ | binOpPrecedence rop <= prec ->
                            "(" ++ rStr ++ ")"

                        _ ->
                            rStr
            in
            ( lFinal ++ " " ++ binOpSymbol op ++ " " ++ rFinal, cg2 )

        ECall (EVar name) [ arg ] ->
            let
                rName =
                    builtinRName name

                ( argStr, cg1 ) =
                    genVectorized cg arg subs
            in
            ( rName ++ "(" ++ argStr ++ ")", cg1 )

        _ ->
            genExpr cg body


-- | Map a Quone builtin name to its R equivalent.
builtinRName :: String -> String
builtinRName name =
    case name of
        "to_double" -> "as.numeric"
        "map" -> "purrr::map_dbl"
        "map2" -> "purrr::map2_dbl"
        "reduce" -> "purrr::reduce"
        "keep" -> "purrr::keep"
        "discard" -> "purrr::discard"
        _ -> name


-- | Generate an expression that goes inside a dplyr verb. Inside dplyr
-- we don't use @<-@ tags or wrap things in @c()@ — column references are
-- bare names.
genColExpr :: Codegen -> Expr -> ( String, Codegen )
genColExpr cg expr =
    case expr of
        EVar name -> ( name, cg )
        EInt n -> ( show n, cg )
        EFloat f -> ( showDouble f, cg )
        EBool b -> ( if b then "TRUE" else "FALSE", cg )
        EStr s -> ( "\"" ++ s ++ "\"", cg )

        EBinary op l r ->
            let
                ( lStr, cg1 ) = genColExpr cg l
                ( rStr, cg2 ) = genColExpr cg1 r
            in
            ( lStr ++ " " ++ binOpSymbol op ++ " " ++ rStr, cg2 )

        ECall func args ->
            let
                ( fStr, cg1 ) =
                    genColExpr cg func

                realArgs =
                    List.filter (not << isUnitExpr) args

                ( argStrs, cg2 ) =
                    mapAccumCg cg1 realArgs genColExpr
            in
            ( fStr ++ "(" ++ List.intercalate ", " argStrs ++ ")", cg2 )

        _ ->
            genExpr cg expr



-- ====================================================================
-- PATTERN MATCHING
-- ====================================================================


-- | Compile a @case ... of@ down to a chain of @if/else if/else@ in R.
-- The scrutinee is bound to a fresh @.tmp_N@ variable so we don't
-- evaluate it more than once.
genMatch :: Codegen -> Expr -> List ( Pattern, Expr ) -> ( String, Codegen )
genMatch cg0 scrutinee arms =
    let
        tmp =
            ".tmp_" ++ show (cgTmpCounter cg0)

        cg1 =
            cg0 { cgTmpCounter = cgTmpCounter cg0 + 1 }

        ( scrutStr, cg2 ) =
            genExpr cg1 scrutinee

        intro =
            tmp ++ " <- " ++ scrutStr ++ "\n"

        emit ( acc, cg' ) ( pat, body ) =
            let
                ( conds, binds ) =
                    compilePattern pat tmp

                ( bodyStr, cg'' ) =
                    genExpr cg' body
            in
            ( acc ++ [ ( conds, binds, bodyStr ) ], cg'' )

        ( compiled, cg3 ) =
            List.foldl (flip emit) ( [], cg2 ) arms

        ( innerCode, _ ) =
            buildMatchCode compiled True False
    in
    ( "local({\n" ++ indentLines 2 (intro ++ innerCode) ++ "\n})", cg3 )


buildMatchCode
    :: List ( List String, List ( String, String ), String )
    -> Bool
    -> Bool
    -> ( String, Bool )
buildMatchCode arms first closed =
    case arms of
        [] ->
            ( "", closed )

        ( conds, binds, bodyStr ) : rest
            | null conds ->
                buildExhaustiveArm first binds bodyStr

            | otherwise ->
                buildConditionalArm conds binds bodyStr rest first closed


buildExhaustiveArm
    :: Bool
    -> List ( String, String )
    -> String
    -> ( String, Bool )
buildExhaustiveArm first binds bodyStr =
    if first then
        let
            bindLines =
                List.concatMap (\( n, p ) -> n ++ " <- " ++ p ++ "\n") binds
        in
        ( bindLines ++ bodyStr, True )
    else
        let
            bindLines =
                List.concatMap (\( n, p ) -> "  " ++ n ++ " <- " ++ p ++ "\n") binds
        in
        ( "} else {\n" ++ bindLines ++ indentLines 2 bodyStr ++ "\n}", True )


buildConditionalArm
    :: List String
    -> List ( String, String )
    -> String
    -> List ( List String, List ( String, String ), String )
    -> Bool
    -> Bool
    -> ( String, Bool )
buildConditionalArm conds binds bodyStr rest first closed =
    let
        cond =
            List.intercalate " && " conds

        bindLines =
            List.concatMap (\( n, p ) -> "  " ++ n ++ " <- " ++ p ++ "\n") binds

        header =
            if first then
                "if (" ++ cond ++ ") {\n"
            else
                "} else if (" ++ cond ++ ") {\n"

        armCode =
            header ++ bindLines ++ indentLines 2 bodyStr ++ "\n"

        ( restCode, closed1 ) =
            buildMatchCode rest False closed
    in
    if null rest && not closed1 then
        ( armCode ++ "} else {\n  stop('Non-exhaustive match')\n}", True )
    else
        ( armCode ++ restCode, closed1 )


-- | Turn a pattern into a list of conditions and a list of bindings.
-- @path@ is the R expression that already evaluates to the scrutinee.
compilePattern :: Pattern -> String -> ( List String, List ( String, String ) )
compilePattern pat path =
    case pat of
        PWildcard ->
            ( [], [] )

        PVar name ->
            ( [], [ ( name, path ) ] )

        PInt n ->
            ( [ "identical(" ++ path ++ ", " ++ show n ++ "L)" ], [] )

        PBool b ->
            ( [ "identical(" ++ path ++ ", " ++ (if b then "TRUE" else "FALSE") ++ ")" ]
            , []
            )

        PConstructor name pats ->
            let
                tagCond =
                    "identical(" ++ path ++ "$`__tag__`, '" ++ name ++ "')"

                ( subConds, subBinds ) =
                    List.unzip
                        [ compilePattern p (path ++ "$field" ++ show (i :: Int))
                        | ( i, p ) <- List.zip [ 1 .. ] pats
                        ]
            in
            ( tagCond : List.concat subConds, List.concat subBinds )



-- ====================================================================
-- LITTLE HELPERS
-- ====================================================================


isUnitExpr :: Expr -> Bool
isUnitExpr EUnit = True
isUnitExpr _ = False


-- | Backslash-escape characters that can't appear raw inside an R string.
escChar :: Char -> String
escChar c =
    case c of
        '\\' -> "\\\\"
        '"' -> "\\\""
        '\n' -> "\\n"
        '\t' -> "\\t"
        _ -> [ c ]


-- | Indent every line of the input by N spaces.
indentLines :: Int -> String -> String
indentLines n s =
    let
        pad =
            replicate n ' '
    in
    unlines
        [ if null line then "" else pad ++ line | line <- lines s ]


-- | Like 'List.foldl' but threading 'Codegen' state and collecting
-- results into a list. Common pattern for code generation.
mapAccumCg
    :: Codegen
    -> List a
    -> (Codegen -> a -> ( b, Codegen ))
    -> ( List b, Codegen )
mapAccumCg cg [] _ =
    ( [], cg )

mapAccumCg cg (x : xs) f =
    let
        ( b, cg1 ) =
            f cg x

        ( bs, cg2 ) =
            mapAccumCg cg1 xs f
    in
    ( b : bs, cg2 )


-- | The R helper that all custom-type constructors call. Emitted once
-- at the top of any file that declares a 'TypeDecl'.
makeVariant :: String
makeVariant =
    unlines
        [ ".make_variant <- function(type, tag, fields = list()) {"
        , "  structure(c(list(`__type__` = type, `__tag__` = tag), fields),"
        , "            class = c(paste0('tl_', type, '_', tag), paste0('tl_', type), 'tl_value'))"
        , "}"
        ]
