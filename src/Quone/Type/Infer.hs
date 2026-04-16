{-|
Module: Quone.Type.Infer

Hindley-Milner type inference, the heart of Quone's type system.

The algorithm in three sentences:

  1. As we walk an expression, every unknown type starts as a fresh
     /type variable/ (a 'TyVar' with a unique integer).
  2. When two types must be equal (function application, @if@ branches,
     pattern matches, ...), we /unify/ them, which extends the substitution
     map.
  3. After processing a top-level binding, we /generalize/ its type by
     turning any free variables into universally quantified ones, producing
     a 'Scheme'. When that binding is used elsewhere, we /instantiate/ the
     scheme with fresh variables.

The 'InferState' carries the next-fresh counter, the substitution, the
typing environment, and the user's type aliases. Every step that might
add to the substitution returns a new 'InferState'.
-}
module Quone.Type.Infer
    ( InferState(..)
    , runInfer
    , inferProgram
    ) where

import qualified Data.Char as Char
import qualified Quone.Dict as Dict
import qualified Quone.List as List
import qualified Quone.Set as Set
import Quone.Dict (Dict)
import Quone.Set (Set)

import Quone.Prelude
import Quone.AST.Source
import Quone.Type.Type



-- ====================================================================
-- STATE
-- ====================================================================


-- | Everything the inference algorithm needs to remember as it walks
-- the program.
data InferState = InferState
    { nextVar :: !Int
      -- ^ The next fresh type-variable id to hand out.

    , subst :: Dict Int Ty
      -- ^ The substitution: a map from variable ids to the type they
      -- have been resolved to.

    , env :: List ( String, Scheme )
      -- ^ The typing environment: name -> polymorphic type. Stored as a
      -- list (not a dict) because we sometimes need to truncate it back
      -- to a previous size when leaving a scope.

    , typeAliases :: Dict String ( List String, List TypeExpr )
      -- ^ User-declared @type alias@ declarations, used when resolving
      -- 'TENamed' references.
    }
    deriving (Show)


newInferState :: InferState
newInferState =
    InferState
        { nextVar = 0
        , subst = Dict.empty
        , env = []
        , typeAliases = Dict.empty
        }


-- | Hand out a brand-new type-variable id.
fresh :: InferState -> ( Int, InferState )
fresh s =
    ( nextVar s, s { nextVar = nextVar s + 1 } )



-- ====================================================================
-- SUBSTITUTION
-- ====================================================================


-- | Walk a type, replacing every variable with whatever the substitution
-- says it should be (transitively).
apply :: InferState -> Ty -> Ty
apply s ty =
    case ty of
        TyVar i ->
            case Dict.get i (subst s) of
                Just resolved ->
                    apply s resolved

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
            TyFunc (List.map (apply s) ps) (apply s r)

        TyAdt n args ->
            TyAdt n (List.map (apply s) args)

        TyRecord fs ->
            TyRecord (List.map (\( n, t ) -> ( n, apply s t )) fs)


-- | Does variable @i@ appear inside @ty@? Used by unification to detect
-- infinite types like @a = List a@.
occurs :: InferState -> Int -> Ty -> Bool
occurs s i ty0 =
    let
        ty =
            apply s ty0
    in
    case ty of
        TyVar j ->
            j == i

        TyInt ->
            False

        TyDouble ->
            False

        TyBool ->
            False

        TyStr ->
            False

        TyFunc ps r ->
            List.any (occurs s i) ps || occurs s i r

        TyAdt _ args ->
            List.any (occurs s i) args

        TyRecord fs ->
            List.any (occurs s i << snd) fs



-- ====================================================================
-- UNIFICATION
-- ====================================================================


-- | Make two types equal by extending the substitution. If they can't
-- be made equal, return an error message.
unify :: InferState -> Ty -> Ty -> Result String InferState
unify s a0 b0 =
    let
        a =
            apply s a0

        b =
            apply s b0
    in
    case ( a, b ) of
        ( TyVar x, TyVar y ) | x == y ->
            Ok s

        ( TyVar x, _ ) ->
            if occurs s x b then
                Err "Infinite type"
            else
                Ok (s { subst = Dict.insert x b (subst s) })

        ( _, TyVar y ) ->
            if occurs s y a then
                Err "Infinite type"
            else
                Ok (s { subst = Dict.insert y a (subst s) })

        ( TyInt, TyInt ) -> Ok s
        ( TyDouble, TyDouble ) -> Ok s
        ( TyBool, TyBool ) -> Ok s
        ( TyStr, TyStr ) -> Ok s

        ( TyFunc p1 r1, TyFunc p2 r2 )
            | length p1 /= length p2 ->
                Err
                    ( "Function arity mismatch: "
                        ++ show (length p1)
                        ++ " vs "
                        ++ show (length p2)
                    )

            | otherwise -> do
                s1 <- foldlR (\st ( x, y ) -> unify st x y) s (List.zip p1 p2)
                unify s1 r1 r2

        ( TyAdt n1 a1, TyAdt n2 a2 )
            | n1 /= n2 ->
                Err ("Type mismatch: " ++ n1 ++ " vs " ++ n2)

            | length a1 /= length a2 ->
                Err ("Type argument count mismatch for " ++ n1)

            | otherwise ->
                foldlR (\st ( x, y ) -> unify st x y) s (List.zip a1 a2)

        ( TyRecord f1, TyRecord f2 ) ->
            unifyRecords s a0 b0 f1 f2

        _ ->
            Err ("Type mismatch: " ++ formatTy a ++ " vs " ++ formatTy b)


-- | Unify two record types. Common fields must unify; missing fields on
-- either side are added (this lets us infer record extension).
unifyRecords
    :: InferState
    -> Ty
    -> Ty
    -> List ( String, Ty )
    -> List ( String, Ty )
    -> Result String InferState
unifyRecords s0 a0 b0 f1 f2 = do
    let
        map1 =
            Dict.fromList f1

        map2 =
            Dict.fromList f2

        -- Unify every field shared between the two records.
        unifyShared st =
            foldlR
                (\acc ( name, t1 ) ->
                    case Dict.get name map2 of
                        Just t2 ->
                            unify acc t1 t2

                        Nothing ->
                            Ok acc
                )
                st
                f1
    s1 <- unifyShared s0
    if differentShape map1 map2 then
        let
            merged =
                List.sortBy (\( a, _ ) ( b, _ ) -> compare a b)
                    (mergeFields f1 f2)

            mergedTy =
                TyRecord (List.map (\( n, t ) -> ( n, apply s1 t )) merged)
        in
        case ( apply s1 a0, apply s1 b0 ) of
            ( TyVar x, _ ) ->
                Ok (s1 { subst = Dict.insert x mergedTy (subst s1) })

            ( _, TyVar y ) ->
                Ok (s1 { subst = Dict.insert y mergedTy (subst s1) })

            _ ->
                Ok s1
    else
        Ok s1


-- | True if the two record-field maps don't have exactly the same key set.
differentShape :: Dict String Ty -> Dict String Ty -> Bool
differentShape m1 m2 =
    Dict.size m1 /= Dict.size m2
        || List.any (\k -> not (Dict.member k m2)) (Dict.keys m1)


-- | Merge two field lists, dropping duplicates from the second list.
mergeFields :: List ( String, Ty ) -> List ( String, Ty ) -> List ( String, Ty )
mergeFields xs ys =
    let
        seen =
            Set.fromList (List.map fst xs)
    in
    xs ++ List.filter (\( n, _ ) -> not (Set.member n seen)) ys


-- | Like 'List.foldl' but with a function that may fail.
foldlR :: (a -> b -> Result e a) -> a -> List b -> Result e a
foldlR _ acc [] =
    Ok acc

foldlR f acc (x : xs) =
    case f acc x of
        Err e ->
            Err e

        Ok acc1 ->
            foldlR f acc1 xs



-- ====================================================================
-- GENERALIZATION & INSTANTIATION
-- ====================================================================


freeVars :: InferState -> Ty -> Set Int
freeVars s ty =
    collectFree (apply s ty)


freeVarsScheme :: InferState -> Scheme -> Set Int
freeVarsScheme s sc =
    let
        tyFree =
            collectFree (apply s (schemeTy sc))

        bound =
            Set.fromList (schemeVars sc)
    in
    Set.diff tyFree bound


-- | Turn a type into a polymorphic 'Scheme' by quantifying over all
-- variables that are free in the type but /not/ free in the environment.
generalize :: InferState -> Ty -> Scheme
generalize s ty0 =
    let
        ty =
            apply s ty0

        freeInTy =
            freeVars s ty

        freeInEnv =
            List.foldl
                (\sc acc -> Set.union acc (freeVarsScheme s sc))
                Set.empty
                (List.map snd (env s))

        vars =
            Set.toList (Set.diff freeInTy freeInEnv)
    in
    Scheme vars ty


-- | Replace every quantified variable in a scheme with a fresh type
-- variable. This is what "use a polymorphic function" actually does:
-- @id : forall a. a -> a@ becomes @t37 -> t37@ at one call site and
-- @t38 -> t38@ at another.
instantiate :: InferState -> Scheme -> ( Ty, InferState )
instantiate s0 sc =
    let
        step ( m, st ) v =
            let
                ( fv, st1 ) =
                    fresh st
            in
            ( Dict.insert v (TyVar fv) m, st1 )

        ( mapping, s1 ) =
            List.foldl (flip step) ( Dict.empty, s0 ) (schemeVars sc)
    in
    ( substScheme (schemeTy sc) mapping, s1 )



-- ====================================================================
-- ENVIRONMENT
-- ====================================================================


lookupEnv :: String -> InferState -> Maybe Scheme
lookupEnv name s =
    case List.find (\( n, _ ) -> n == name) (List.reverse (env s)) of
        Just ( _, sc ) ->
            Just sc

        Nothing ->
            Nothing


pushEnv :: String -> Scheme -> InferState -> InferState
pushEnv name sc s =
    s { env = env s ++ [ ( name, sc ) ] }


truncateEnv :: Int -> InferState -> InferState
truncateEnv n s =
    s { env = List.take n (env s) }



-- ====================================================================
-- TYPE EXPRESSIONS  ->  Ty
-- ====================================================================


-- | Resolve a 'TypeExpr' from the source AST into an internal 'Ty'.
-- The 'Dict String Int' maps the user's type-variable names ("a", "b")
-- to fresh type-variable ids.
typeExprToTy
    :: InferState
    -> TypeExpr
    -> Dict String Int
    -> Result String ( Ty, InferState )
typeExprToTy s te params =
    case te of
        TEVar name ->
            case Dict.get name params of
                Just i ->
                    Ok ( TyVar i, s )

                Nothing ->
                    Err ("Unknown type variable: " ++ name)

        TENamed name args -> do
            ( argTys, s1 ) <-
                mapAccum s args (\st a -> typeExprToTy st a params)
            resolveNamedType s1 name argTys params

        TEFunc paramExprs retExpr -> do
            ( paramTys, s1 ) <-
                mapAccum s paramExprs (\st p -> typeExprToTy st p params)
            ( retTy, s2 ) <- typeExprToTy s1 retExpr params
            Ok ( TyFunc paramTys retTy, s2 )

        TERecord fields ->
            buildRecordTy s fields params

        TEDataframe fields ->
            buildRecordTy s fields params


buildRecordTy
    :: InferState
    -> List ( String, TypeExpr )
    -> Dict String Int
    -> Result String ( Ty, InferState )
buildRecordTy s fields params = do
    ( fieldTys, s1 ) <-
        mapAccum s fields
            (\st ( n, te ) -> do
                ( ty, st1 ) <- typeExprToTy st te params
                Ok ( ( n, ty ), st1 )
            )
    let
        sorted =
            List.sortBy (\( a, _ ) ( b, _ ) -> compare a b) fieldTys
    Ok ( TyRecord sorted, s1 )


-- | A 'TENamed' could be a primitive type, a type alias, or just an ADT
-- application. Try each in turn.
resolveNamedType
    :: InferState
    -> String
    -> List Ty
    -> Dict String Int
    -> Result String ( Ty, InferState )
resolveNamedType s1 name argTys params =
    case name of
        "Integer" ->
            Ok ( TyInt, s1 )

        "Double" ->
            Ok ( TyDouble, s1 )

        "Logical" ->
            Ok ( TyBool, s1 )

        "Character" ->
            Ok ( TyStr, s1 )

        _ ->
            case Dict.get name (typeAliases s1) of
                Just ( aliasParams, aliasBody ) ->
                    expandAlias s1 aliasParams aliasBody argTys params

                Nothing ->
                    Ok ( TyAdt name argTys, s1 )


expandAlias
    :: InferState
    -> List String
    -> List TypeExpr
    -> List Ty
    -> Dict String Int
    -> Result String ( Ty, InferState )
expandAlias s1 aliasParams aliasBody argTys params = do
    let
        bind ( m, st ) ( pn, argTy ) =
            let
                ( fv, st1 ) =
                    fresh st

                st2 =
                    st1 { subst = Dict.insert fv argTy (subst st1) }
            in
            ( Dict.insert pn fv m, st2 )

        ( aliasMap, s2 ) =
            List.foldl (flip bind) ( params, s1 ) (List.zip aliasParams argTys)
    ( tys, s3 ) <-
        mapAccum s2 aliasBody (\st te -> typeExprToTy st te aliasMap)
    case tys of
        [ single ] ->
            Ok ( single, s3 )

        _ ->
            let
                ret =
                    last tys

                ps =
                    init tys
            in
            Ok ( TyFunc ps ret, s3 )


-- | Like 'List.foldl' but threading state through a function that may fail.
mapAccum
    :: s
    -> List a
    -> (s -> a -> Result e ( b, s ))
    -> Result e ( List b, s )
mapAccum s0 [] _ =
    Ok ( [], s0 )

mapAccum s0 (x : xs) f = do
    ( b, s1 ) <- f s0 x
    ( bs, s2 ) <- mapAccum s1 xs f
    Ok ( b : bs, s2 )



-- ====================================================================
-- BUILTINS
-- ====================================================================


-- | Pre-populate the environment with @map@, @map2@, @reduce@, @keep@,
-- @discard@. We handcraft their schemes so they're available without an
-- explicit import.
registerBuiltins :: InferState -> InferState
registerBuiltins s0 =
    s0
        |> registerBuiltin "map" registerMap
        |> registerBuiltin "map2" registerMap2
        |> registerBuiltin "reduce" registerReduce
        |> registerBuiltin "keep" registerKeep
        |> registerBuiltin "discard" registerDiscard


registerBuiltin
    :: String
    -> (InferState -> ( Scheme, InferState ))
    -> InferState
    -> InferState
registerBuiltin name make s =
    let
        ( scheme, s1 ) =
            make s
    in
    pushEnv name scheme s1


registerMap :: InferState -> ( Scheme, InferState )
registerMap s =
    let
        ( a, s1 ) = fresh s
        ( b, s2 ) = fresh s1

        vecA = TyAdt "Vector" [ TyVar a ]
        vecB = TyAdt "Vector" [ TyVar b ]
        funAB = TyFunc [ TyVar a ] (TyVar b)
    in
    ( Scheme [ a, b ] (TyFunc [ funAB, vecA ] vecB), s2 )


registerMap2 :: InferState -> ( Scheme, InferState )
registerMap2 s =
    let
        ( a, s1 ) = fresh s
        ( b, s2 ) = fresh s1
        ( c, s3 ) = fresh s2

        vecA = TyAdt "Vector" [ TyVar a ]
        vecB = TyAdt "Vector" [ TyVar b ]
        vecC = TyAdt "Vector" [ TyVar c ]
        funABC = TyFunc [ TyVar a, TyVar b ] (TyVar c)
    in
    ( Scheme [ a, b, c ] (TyFunc [ funABC, vecA, vecB ] vecC), s3 )


registerReduce :: InferState -> ( Scheme, InferState )
registerReduce s =
    let
        ( a, s1 ) = fresh s
        ( b, s2 ) = fresh s1

        vecA = TyAdt "Vector" [ TyVar a ]
        funBAB = TyFunc [ TyVar b, TyVar a ] (TyVar b)
    in
    ( Scheme [ a, b ] (TyFunc [ funBAB, TyVar b, vecA ] (TyVar b)), s2 )


registerKeep :: InferState -> ( Scheme, InferState )
registerKeep s =
    let
        ( a, s1 ) = fresh s
        vecA = TyAdt "Vector" [ TyVar a ]
        pred = TyFunc [ TyVar a ] TyBool
    in
    ( Scheme [ a ] (TyFunc [ pred, vecA ] vecA), s1 )


registerDiscard :: InferState -> ( Scheme, InferState )
registerDiscard s =
    let
        ( a, s1 ) = fresh s
        vecA = TyAdt "Vector" [ TyVar a ]
        pred = TyFunc [ TyVar a ] TyBool
    in
    ( Scheme [ a ] (TyFunc [ pred, vecA ] vecA), s1 )



-- ====================================================================
-- DECLARATIONS
-- ====================================================================


-- | Add every variant of a 'TypeDecl' to the environment as a constructor
-- function (or a value, for nullary constructors).
registerType :: InferState -> TypeDecl -> Result String InferState
registerType s0 td = do
    let
        register ( m, vs, st ) pn =
            let
                ( tv, st1 ) =
                    fresh st
            in
            ( Dict.insert pn tv m, vs ++ [ tv ], st1 )

        ( paramMap, typeVars, s1 ) =
            List.foldl
                (flip register)
                ( Dict.empty, [], s0 )
                (typeParams td)

        resultTy =
            TyAdt (typeName td) (List.map TyVar typeVars)

        addVariant st v =
            let
                ctorTy =
                    if null (variantFields v) then
                        resultTy
                    else
                        case mapAccum st (variantFields v) (\sx f -> typeExprToTy sx f paramMap) of
                            Err e ->
                                error e

                            Ok ( fts, _ ) ->
                                TyFunc fts resultTy

                scheme =
                    Scheme typeVars ctorTy
            in
            Ok (pushEnv (variantName v) scheme st)
    foldlR addVariant s1 (typeVariants td)


-- | Add an @import@ed function to the environment.
registerExtern :: InferState -> ExternDecl -> Result String InferState
registerExtern s0 ed = do
    let
        ( paramMap, s1 ) =
            collectTypeVarsFromExprs s0 (externTyExpr ed)
    ( tys, s2 ) <-
        mapAccum s1 (externTyExpr ed)
            (\st te -> typeExprToTy st te paramMap)
    let
        vars =
            Dict.values paramMap

        ty =
            case tys of
                [ single ] ->
                    single

                _ ->
                    TyFunc (init tys) (last tys)
    Ok (pushEnv (externName ed) (Scheme vars ty) s2)


-- | Walk type expressions and collect every type-variable name (like @a@,
-- @b@), assigning each a fresh id.
collectTypeVarsFromExprs
    :: InferState
    -> List TypeExpr
    -> ( Dict String Int, InferState )
collectTypeVarsFromExprs s0 tes =
    List.foldl (flip collectOne) ( Dict.empty, s0 ) tes
  where
    collectOne ( m, s ) te =
        case te of
            TEVar name
                | Dict.member name m ->
                    ( m, s )

                | otherwise ->
                    let
                        ( fv, s1 ) =
                            fresh s
                    in
                    ( Dict.insert name fv m, s1 )

            TENamed _ args ->
                List.foldl (flip collectOne) ( m, s ) args

            TEFunc ps r ->
                let
                    ( m1, s1 ) =
                        List.foldl (flip collectOne) ( m, s ) ps
                in
                collectOne ( m1, s1 ) r

            TERecord fs ->
                List.foldl
                    (\( n, t ) acc -> collectOne acc t |> stripField n)
                    ( m, s )
                    fs

            TEDataframe fs ->
                List.foldl
                    (\( n, t ) acc -> collectOne acc t |> stripField n)
                    ( m, s )
                    fs

    -- Field name is unused; we only care about the recursive call.
    stripField _ acc =
        acc


-- | Resolve a parsed annotation (a list of type expressions joined by
-- arrows) into a single 'Ty', possibly wrapping in a 'TyFunc'.
annotationToTy :: InferState -> List TypeExpr -> Result String ( Ty, InferState )
annotationToTy s0 parts = do
    let
        ( paramMap, s1 ) =
            collectTypeVarsFromExprs s0 parts
    ( tys, s2 ) <-
        mapAccum s1 parts (\st te -> typeExprToTy st te paramMap)
    case tys of
        [ single ] ->
            Ok ( single, s2 )

        _ ->
            Ok ( TyFunc (init tys) (last tys), s2 )



-- ====================================================================
-- EXPRESSIONS
-- ====================================================================


-- | Infer the type of a single expression.
inferExpr :: InferState -> Expr -> Result String ( Ty, InferState )
inferExpr s expr =
    case expr of
        EVar name ->
            case lookupEnv name s of
                Just sc ->
                    let
                        ( ty, s1 ) =
                            instantiate s sc
                    in
                    Ok ( ty, s1 )

                Nothing ->
                    Err ("Unbound variable: " ++ name)

        EInt _ ->
            Ok ( TyInt, s )

        EFloat _ ->
            Ok ( TyDouble, s )

        EBool _ ->
            Ok ( TyBool, s )

        EStr _ ->
            Ok ( TyStr, s )

        EUnit ->
            let
                ( v, s1 ) =
                    fresh s
            in
            Ok ( TyVar v, s1 )

        EVecLit elems ->
            inferVecLit s elems

        ELambda params body ->
            inferLambda s params body

        ECall func args ->
            inferCall s func args

        EIf cond thenE elseE ->
            inferIf s cond thenE elseE

        EMatch scrut arms ->
            inferMatch s scrut arms

        EBinary op l r ->
            inferBinary s op l r

        ELet bindings body ->
            inferLet s bindings body

        EDplyr _ _ ->
            -- Dplyr expressions are checked specially through pipes;
            -- standalone they're given a fresh variable so the program
            -- still type-checks if a user does odd things.
            let
                ( v, s1 ) =
                    fresh s
            in
            Ok ( TyVar v, s1 )

        ERecord fields ->
            inferRecord s fields

        EDataFrame fields ->
            inferRecord s fields

        EFieldAccess inner field ->
            inferFieldAccess s inner field

        EPipe left right ->
            inferPipe s left right


inferVecLit :: InferState -> List Expr -> Result String ( Ty, InferState )
inferVecLit s elems = do
    let
        ( elemVar, s1 ) =
            fresh s

        elemTy =
            TyVar elemVar
    s2 <-
        foldlR
            (\st e -> do
                ( t, st1 ) <- inferExpr st e
                unify st1 elemTy t
            )
            s1
            elems
    Ok ( TyAdt "Vector" [ apply s2 elemTy ], s2 )


inferLambda
    :: InferState -> List String -> Expr -> Result String ( Ty, InferState )
inferLambda s params body = do
    let
        addParam ( tys, st ) _ =
            let
                ( fv, st1 ) =
                    fresh st
            in
            ( tys ++ [ TyVar fv ], st1 )

        ( paramTys, s1 ) =
            List.foldl (flip addParam) ( [], s ) params

        prev =
            length (env s1)

        s2 =
            List.foldl
                (\( name, ty ) st -> pushEnv name (Scheme [] ty) st)
                s1
                (List.zip params paramTys)
    ( retTy, s3 ) <- inferExpr s2 body
    let
        s4 =
            truncateEnv prev s3
    Ok ( TyFunc paramTys retTy, s4 )


inferCall :: InferState -> Expr -> List Expr -> Result String ( Ty, InferState )
inferCall s func args = do
    ( funcTy, s1 ) <- inferExpr s func
    let
        realArgs =
            List.filter (not << isUnit) args
    if null realArgs then
        let
            ( rv, s2 ) =
                fresh s1
        in
        Ok ( TyVar rv, s2 )
    else do
        ( argTys, s2 ) <- mapAccum s1 realArgs inferExpr
        let
            ( retVar, s3 ) =
                fresh s2

            expected =
                TyFunc argTys (TyVar retVar)
        s4 <- unify s3 funcTy expected
        Ok ( TyVar retVar, s4 )


inferIf :: InferState -> Expr -> Expr -> Expr -> Result String ( Ty, InferState )
inferIf s cond thenE elseE = do
    ( ct, s1 ) <- inferExpr s cond
    s2 <- unify s1 ct TyBool
    ( tt, s3 ) <- inferExpr s2 thenE
    ( et, s4 ) <- inferExpr s3 elseE
    s5 <- unify s4 tt et
    Ok ( tt, s5 )


inferMatch
    :: InferState
    -> Expr
    -> List ( Pattern, Expr )
    -> Result String ( Ty, InferState )
inferMatch s scrutinee arms = do
    ( scrutTy, s1 ) <- inferExpr s scrutinee
    let
        ( resultVar, s2 ) =
            fresh s1

        resultTy =
            TyVar resultVar

        checkArm st ( pat, body ) = do
            let
                prev =
                    length (env st)
            st1 <- checkPattern st pat scrutTy
            ( bodyTy, st2 ) <- inferExpr st1 body
            st3 <- unify st2 resultTy bodyTy
            Ok (truncateEnv prev st3)
    s3 <- foldlR checkArm s2 arms
    Ok ( resultTy, s3 )


inferBinary
    :: InferState -> BinOp -> Expr -> Expr -> Result String ( Ty, InferState )
inferBinary s op l r = do
    ( lt, s1 ) <- inferExpr s l
    ( rt, s2 ) <- inferExpr s1 r
    case op of
        Add ->
            sameType s2 lt rt

        Sub ->
            sameType s2 lt rt

        Mul ->
            sameType s2 lt rt

        Div -> do
            s3 <- unify s2 lt TyDouble
            s4 <- unify s3 rt TyDouble
            Ok ( TyDouble, s4 )

        _ -> do
            s3 <- unify s2 lt rt
            Ok ( TyBool, s3 )


sameType :: InferState -> Ty -> Ty -> Result String ( Ty, InferState )
sameType s lt rt = do
    s1 <- unify s lt rt
    Ok ( apply s1 lt, s1 )


inferLet
    :: InferState
    -> List ( String, Expr )
    -> Expr
    -> Result String ( Ty, InferState )
inferLet s bindings body = do
    let
        prev =
            length (env s)

        addBinding st ( name, bindExpr ) = do
            ( ty, st1 ) <- inferExpr st bindExpr
            if name == "_" || '.' `elem` name then
                Ok st1
            else
                let
                    ty1 =
                        apply st1 ty

                    sc =
                        generalize st1 ty1
                in
                Ok (pushEnv name sc st1)
    s1 <- foldlR addBinding s bindings
    ( result, s2 ) <- inferExpr s1 body
    Ok ( result, truncateEnv prev s2 )


inferRecord
    :: InferState
    -> List ( String, Expr )
    -> Result String ( Ty, InferState )
inferRecord s fields = do
    ( fieldTys, s1 ) <-
        mapAccum s fields
            (\st ( n, e ) -> do
                ( ty, st1 ) <- inferExpr st e
                Ok ( ( n, ty ), st1 )
            )
    let
        sorted =
            List.sortBy (\( a, _ ) ( b, _ ) -> compare a b) fieldTys
    Ok ( TyRecord sorted, s1 )


inferFieldAccess
    :: InferState -> Expr -> String -> Result String ( Ty, InferState )
inferFieldAccess s inner field = do
    ( rawTy, s1 ) <- inferExpr s inner
    let
        resolved =
            apply s1 rawTy
    case resolved of
        TyRecord fields ->
            case lookup field fields of
                Just ty ->
                    Ok ( ty, s1 )

                Nothing ->
                    extendRecordWithField s1 rawTy fields field

        TyVar i -> do
            let
                ( fv, s2 ) =
                    fresh s1

                fieldTy =
                    TyVar fv

                rec =
                    TyRecord [ ( field, fieldTy ) ]
            Ok
                ( fieldTy
                , s2 { subst = Dict.insert i rec (subst s2) }
                )

        _ ->
            Err ("Cannot access field '" ++ field ++ "' on non-record type")


extendRecordWithField
    :: InferState
    -> Ty
    -> List ( String, Ty )
    -> String
    -> Result String ( Ty, InferState )
extendRecordWithField s rawTy fields field =
    case rawTy of
        TyVar i ->
            let
                ( fv, s1 ) =
                    fresh s

                fieldTy =
                    TyVar fv

                newFields =
                    List.sortBy
                        (\( a, _ ) ( b, _ ) -> compare a b)
                        (fields ++ [ ( field, fieldTy ) ])
            in
            Ok
                ( fieldTy
                , s1 { subst = Dict.insert i (TyRecord newFields) (subst s1) }
                )

        _ ->
            Err ("Record has no field '" ++ field ++ "'")


-- | The pipe operator handles three special cases:
--
--   1. @x |> f@ where @f@ is a call: append @x@ to the call's argument list
--   2. @x |> f@ where @f@ is a variable: turn it into a call with @x@
--   3. @df |> select c1 c2@ (etc.): special dplyr handling that propagates
--      the dataframe schema through the verb
inferPipe :: InferState -> Expr -> Expr -> Result String ( Ty, InferState )
inferPipe s left right =
    case right of
        ECall f args ->
            inferExpr s (ECall f (args ++ [ left ]))

        EVar _ ->
            inferExpr s (ECall right [ left ])

        EDplyr verb dArgs -> do
            ( leftTy, s1 ) <- inferExpr s left
            let
                resolved =
                    apply s1 leftTy
            case resolved of
                TyRecord fields -> do
                    s2 <- checkDplyrStep s1 verb dArgs fields
                    let
                        resultTy =
                            evolveDplyrSchema s2 verb dArgs fields
                    Ok ( resultTy, s2 )

                _ ->
                    inferExpr s1 right

        _ ->
            Err "Right side of |> must be a function or call"


isUnit :: Expr -> Bool
isUnit EUnit = True
isUnit _ = False



-- ====================================================================
-- PATTERNS
-- ====================================================================


-- | Make a pattern's variables available in the environment, and check
-- that the pattern matches the scrutinee's type.
checkPattern :: InferState -> Pattern -> Ty -> Result String InferState
checkPattern s pat expected =
    case pat of
        PWildcard ->
            Ok s

        PVar name ->
            let
                resolved =
                    apply s expected
            in
            Ok (pushEnv name (Scheme [] resolved) s)

        PInt _ ->
            unify s expected TyInt

        PBool _ ->
            unify s expected TyBool

        PConstructor name pats ->
            checkConstructorPattern s name pats expected


checkConstructorPattern
    :: InferState -> String -> List Pattern -> Ty -> Result String InferState
checkConstructorPattern s name pats expected = do
    sc <-
        case lookupEnv name s of
            Just sc1 ->
                Ok sc1

            Nothing ->
                Err ("Unknown constructor: " ++ name)
    let
        ( ctorTy, s1 ) =
            instantiate s sc
    if null pats then
        unify s1 ctorTy expected
    else
        case apply s1 ctorTy of
            TyFunc fieldTys resultTy
                | length fieldTys /= length pats ->
                    Err
                        ( "Constructor "
                            ++ name
                            ++ " expects "
                            ++ show (length fieldTys)
                            ++ " fields, got "
                            ++ show (length pats)
                        )

                | otherwise -> do
                    s2 <- unify s1 resultTy expected
                    foldlR
                        (\st ( p, ft ) -> checkPattern st p ft)
                        s2
                        (List.zip pats fieldTys)

            _ ->
                Err ("Constructor " ++ name ++ " is not a function type")



-- ====================================================================
-- DPLYR
-- ====================================================================


-- | Verify that every argument to a dplyr verb refers to a real column
-- (or a known function).
checkDplyrStep
    :: InferState
    -> String
    -> List DplyrArg
    -> List ( String, Ty )
    -> Result String InferState
checkDplyrStep s0 verb args schema =
    let
        colNames =
            List.map fst schema

        check st arg =
            case arg of
                DplyrColumn name ->
                    if List.member name colNames then
                        Ok st
                    else
                        Err
                            ( "Unknown column '"
                                ++ name
                                ++ "' in "
                                ++ verb
                                ++ ". Available columns: "
                                ++ List.intercalate ", " colNames
                            )

                DplyrPred dExpr -> do
                    _ <- inferDplyrColExpr st dExpr schema verb
                    Ok st

                DplyrNamed _ dExpr -> do
                    _ <- inferDplyrColExpr st dExpr schema verb
                    Ok st
    in
    foldlR check s0 args


-- | Infer the type of an expression that appears inside a dplyr verb,
-- where bare identifiers refer to columns rather than variables.
inferDplyrColExpr
    :: InferState
    -> Expr
    -> List ( String, Ty )
    -> String
    -> Result String Ty
inferDplyrColExpr s expr schema verb =
    case expr of
        EVar name ->
            case List.find (\( n, _ ) -> n == name) schema of
                Just ( _, ty ) ->
                    Ok (unwrapVector ty)

                Nothing ->
                    case lookupEnv name s of
                        Just sc ->
                            let
                                resolved =
                                    apply s (fst (instantiate s sc))
                            in
                            case resolved of
                                TyFunc _ _ ->
                                    Ok resolved

                                _ ->
                                    Err (unknownColumnError name verb schema)

                        Nothing ->
                            Err (unknownColumnError name verb schema)

        EBinary op l r ->
            inferDplyrBinary s op l r schema verb

        ECall func args ->
            inferDplyrCall s func args schema verb

        EInt _ -> Ok TyInt
        EFloat _ -> Ok TyDouble
        EBool _ -> Ok TyBool
        EStr _ -> Ok TyStr

        _ ->
            Ok TyDouble


unknownColumnError :: String -> String -> List ( String, Ty ) -> String
unknownColumnError name verb schema =
    "Unknown column '"
        ++ name
        ++ "' in "
        ++ verb
        ++ ". Available columns: "
        ++ List.intercalate ", " (List.map fst schema)


inferDplyrBinary
    :: InferState
    -> BinOp
    -> Expr
    -> Expr
    -> List ( String, Ty )
    -> String
    -> Result String Ty
inferDplyrBinary s op l r schema verb = do
    lt <- inferDplyrColExpr s l schema verb
    rt <- inferDplyrColExpr s r schema verb
    let
        opSym =
            binOpSymbol op

        numErr name1 =
            "Type mismatch in "
                ++ verb
                ++ ": '"
                ++ name1
                ++ "' is Character, cannot use '"
                ++ opSym
                ++ "'"

        checkNotStr ty side =
            case ty of
                TyStr ->
                    Err (numErr (exprName side))

                _ ->
                    Ok ()
    case op of
        Div -> do
            _ <- checkNotStr lt l
            _ <- checkNotStr rt r
            Ok TyDouble

        Eq -> Ok TyBool
        Neq -> Ok TyBool
        Gt -> Ok TyBool
        Lt -> Ok TyBool
        GtEq -> Ok TyBool
        LtEq -> Ok TyBool

        Add -> do
            _ <- checkNotStr lt l
            _ <- checkNotStr rt r
            Ok lt

        Sub -> do
            _ <- checkNotStr lt l
            _ <- checkNotStr rt r
            Ok lt

        Mul -> do
            _ <- checkNotStr lt l
            _ <- checkNotStr rt r
            Ok lt


inferDplyrCall
    :: InferState
    -> Expr
    -> List Expr
    -> List ( String, Ty )
    -> String
    -> Result String Ty
inferDplyrCall s func args schema verb =
    let
        realArgs =
            List.filter (not << isUnit) args
    in
    case func of
        EVar fname ->
            case lookupEnv fname s of
                Just sc ->
                    let
                        ( ty, s1 ) =
                            instantiate s sc
                    in
                    case apply s1 ty of
                        TyFunc _ retTy ->
                            Ok (apply s1 retTy)

                        resolved ->
                            Ok resolved

                Nothing ->
                    case realArgs of
                        [ a ] ->
                            inferDplyrColExpr s a schema verb

                        _ ->
                            Ok TyDouble

        _ ->
            Ok TyDouble


exprName :: Expr -> String
exprName e =
    case e of
        EVar n -> n
        EInt n -> show n
        EFloat f -> show f
        EBool b -> if b then "True" else "False"
        EStr s -> "\"" ++ s ++ "\""
        _ -> "<expr>"


unwrapVector :: Ty -> Ty
unwrapVector ty =
    case ty of
        TyAdt "Vector" [ inner ] ->
            inner

        other ->
            other


-- | Compute the new dataframe schema after a dplyr step.
evolveDplyrSchema
    :: InferState -> String -> List DplyrArg -> List ( String, Ty ) -> Ty
evolveDplyrSchema s verb args schema =
    case verb of
        "select" ->
            let
                selected =
                    [ n | DplyrColumn n <- args ]
            in
            if null selected then
                TyRecord schema
            else
                TyRecord
                    ( List.sortBy
                        (\( a, _ ) ( b, _ ) -> compare a b)
                        (List.filter (\( n, _ ) -> List.member n selected) schema)
                    )

        "group_by" ->
            TyRecord schema

        "arrange" ->
            TyRecord schema

        "filter" ->
            TyRecord schema

        "mutate" ->
            evolveMutate s args schema verb

        "summarize" ->
            evolveSummarize s args schema verb

        _ ->
            TyRecord schema


evolveMutate :: InferState -> List DplyrArg -> List ( String, Ty ) -> String -> Ty
evolveMutate s args schema verb =
    let
        addOrReplace fs arg =
            case arg of
                DplyrNamed name dExpr ->
                    let
                        colTy =
                            case inferDplyrColExpr s dExpr schema verb of
                                Ok ty ->
                                    TyAdt "Vector" [ ty ]

                                Err _ ->
                                    TyAdt "Vector" [ TyDouble ]
                    in
                    replaceOrAdd name colTy fs

                _ ->
                    fs

        newFields =
            List.foldl (flip addOrReplace) schema args
    in
    TyRecord (List.sortBy (\( a, _ ) ( b, _ ) -> compare a b) newFields)


evolveSummarize :: InferState -> List DplyrArg -> List ( String, Ty ) -> String -> Ty
evolveSummarize s args schema verb =
    let
        colTy dExpr =
            case inferDplyrColExpr s dExpr schema verb of
                Ok ty ->
                    TyAdt "Vector" [ ty ]

                Err _ ->
                    TyAdt "Vector" [ TyDouble ]

        fields =
            [ ( n, colTy dExpr ) | DplyrNamed n dExpr <- args ]
    in
    TyRecord (List.sortBy (\( a, _ ) ( b, _ ) -> compare a b) fields)


replaceOrAdd :: String -> Ty -> List ( String, Ty ) -> List ( String, Ty )
replaceOrAdd name ty fs =
    if List.any (\( n, _ ) -> n == name) fs then
        List.map (\( n, t ) -> if n == name then ( n, ty ) else ( n, t )) fs
    else
        fs ++ [ ( name, ty ) ]



-- ====================================================================
-- THE TOP-LEVEL DRIVER
-- ====================================================================


-- | Walk a 'Program' and return the inferred type of every top-level
-- @let@-binding, together with the final 'InferState'.
inferProgram :: InferState -> Program -> Result String ( List ( String, Ty ), InferState )
inferProgram s0 prog = do
    let
        s1 =
            registerBuiltins s0

        registerDecl st d =
            case d of
                DeclType td ->
                    registerType st td

                DeclTypeAlias ta ->
                    Ok (registerAlias st ta)

                DeclImport ed ->
                    registerExtern st ed

                _ ->
                    Ok st
    s2 <- foldlR registerDecl s1 (progDecls prog)
    ( types, s3 ) <-
        foldlR (inferDecl prog) ( [], s2 ) (progDecls prog)
            |> fmap (\( ts, st ) -> ( ts, st ))
    let
        finalTypes =
            List.map (\( n, t ) -> ( n, apply s3 t )) types
    Ok ( finalTypes, s3 )
  where
    fmap f result =
        case result of
            Ok x ->
                Ok (f x)

            Err e ->
                Err e


-- | Wrap a single declaration's inference, threading the (types-so-far,
-- state) tuple.
inferDecl
    :: Program
    -> ( List ( String, Ty ), InferState )
    -> Decl
    -> Result String ( List ( String, Ty ), InferState )
inferDecl _prog ( tys, st ) d =
    case d of
        DeclLet ld -> do
            ( ty, st1 ) <- inferLetDecl st ld
            Ok ( tys ++ [ ( letName ld, ty ) ], st1 )

        _ ->
            Ok ( tys, st )


-- | Register a type alias in the inference state.
registerAlias :: InferState -> TypeAliasDecl -> InferState
registerAlias st ta =
    st
        { typeAliases =
            Dict.insert
                (aliasName ta)
                ( aliasParams ta, aliasBody ta )
                (typeAliases st)
        }


-- | Type-check a single value binding (and add the binding to the env).
inferLetDecl :: InferState -> LetDecl -> Result String ( Ty, InferState )
inferLetDecl st ld = do
    let
        ( preVar, st1 ) =
            fresh st

        st2 =
            pushEnv (letName ld) (Scheme [] (TyVar preVar)) st1

        wrap :: String -> String
        wrap e =
            "In definition of '" ++ letName ld ++ "': " ++ e

        wrapErr :: Result String a -> Result String a
        wrapErr r =
            case r of
                Ok x ->
                    Ok x

                Err e ->
                    Err (wrap e)
    ( ty, st3 ) <- wrapErr (inferExpr st2 (letExpr ld))
    st4 <- wrapErr (unify st3 (TyVar preVar) ty)
    st5 <-
        case letAnnotation ld of
            Just ann -> do
                ( annTy, st4a ) <- annotationToTy st4 ann
                wrapErr (unify st4a ty annTy)

            Nothing ->
                Ok st4
    let
        ty1 =
            apply st5 ty

        st6 =
            st5 { env = init (env st5) }

        sc =
            generalize st6 ty1

        st7 =
            pushEnv (letName ld) sc st6
    Ok ( ty1, st7 )


runInfer :: Program -> Result String ( List ( String, Ty ), InferState )
runInfer =
    inferProgram newInferState
