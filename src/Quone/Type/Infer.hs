module Quone.Type.Infer
  ( InferState(..)
  , runInfer
  , inferProgram
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (sortBy, find, intercalate)
import Data.Ord (comparing)

import Quone.AST.Source
import Quone.Type.Type

data InferState = InferState
  { nextVar     :: !Int
  , subst       :: Map.Map Int Ty
  , env         :: [(String, Scheme)]
  , typeAliases :: Map.Map String ([String], [TypeExpr])
  } deriving (Show)

newInferState :: InferState
newInferState = InferState 0 Map.empty [] Map.empty

fresh :: InferState -> (Int, InferState)
fresh s = (nextVar s, s { nextVar = nextVar s + 1 })

apply :: InferState -> Ty -> Ty
apply s ty = case ty of
  TyVar i -> case Map.lookup i (subst s) of
    Just resolved -> apply s resolved
    Nothing       -> TyVar i
  TyInt    -> TyInt
  TyDouble -> TyDouble
  TyBool   -> TyBool
  TyStr    -> TyStr
  TyFunc ps r  -> TyFunc (map (apply s) ps) (apply s r)
  TyAdt n args -> TyAdt n (map (apply s) args)
  TyRecord fs  -> TyRecord (map (\(n, t) -> (n, apply s t)) fs)

occurs :: InferState -> Int -> Ty -> Bool
occurs s i ty0 =
  let ty = apply s ty0
  in case ty of
    TyVar j      -> j == i
    TyInt        -> False
    TyDouble     -> False
    TyBool       -> False
    TyStr        -> False
    TyFunc ps r  -> any (occurs s i) ps || occurs s i r
    TyAdt _ args -> any (occurs s i) args
    TyRecord fs  -> any (occurs s i . snd) fs

unify :: InferState -> Ty -> Ty -> Either String InferState
unify s a0 b0 =
  let a = apply s a0
      b = apply s b0
  in case (a, b) of
    (TyVar x, TyVar y) | x == y -> Right s
    (TyVar x, _) ->
      if occurs s x b then Left "Infinite type"
      else Right s { subst = Map.insert x b (subst s) }
    (_, TyVar y) ->
      if occurs s y a then Left "Infinite type"
      else Right s { subst = Map.insert y a (subst s) }
    (TyInt, TyInt)       -> Right s
    (TyDouble, TyDouble) -> Right s
    (TyBool, TyBool)     -> Right s
    (TyStr, TyStr)       -> Right s
    (TyFunc p1 r1, TyFunc p2 r2)
      | length p1 /= length p2 ->
        Left $ "Function arity mismatch: " ++ show (length p1) ++ " vs " ++ show (length p2)
      | otherwise -> do
        s' <- foldM (\st (x, y) -> unify st x y) s (zip p1 p2)
        unify s' r1 r2
    (TyAdt n1 a1, TyAdt n2 a2)
      | n1 /= n2 -> Left $ "Type mismatch: " ++ n1 ++ " vs " ++ n2
      | length a1 /= length a2 -> Left $ "Type argument count mismatch for " ++ n1
      | otherwise -> foldM (\st (x, y) -> unify st x y) s (zip a1 a2)
    (TyRecord f1, TyRecord f2) -> unifyRecords s a0 b0 f1 f2
    _ -> Left $ "Type mismatch: " ++ formatTy a ++ " vs " ++ formatTy b

unifyRecords :: InferState -> Ty -> Ty -> [(String, Ty)] -> [(String, Ty)] -> Either String InferState
unifyRecords s0 a0 b0 f1 f2 = do
  let map1 = Map.fromList f1
      map2 = Map.fromList f2
  s1 <- Map.foldlWithKey' (\acc name t1 ->
    acc >>= \st -> case Map.lookup name map2 of
      Just t2 -> unify st t1 t2
      Nothing -> Right st
    ) (Right s0) map1
  if Map.size map1 /= Map.size map2 || any (\k -> not (Map.member k map2)) (Map.keys map1)
    then let seen = Set.empty
             merged = mergeSorted f1 f2 seen
             mergedSorted = sortBy (comparing fst) merged
             mergedTy = TyRecord (map (\(n, t) -> (n, apply s1 t)) mergedSorted)
         in case (apply s1 a0, apply s1 b0) of
              (TyVar x, _) -> Right s1 { subst = Map.insert x mergedTy (subst s1) }
              (_, TyVar y) -> Right s1 { subst = Map.insert y mergedTy (subst s1) }
              _ -> Right s1
    else Right s1
  where
    mergeSorted [] ys _ = ys
    mergeSorted ((n,t):xs) ys seen =
      if Set.member n seen
      then mergeSorted xs ys seen
      else (n,t) : mergeSorted xs ys (Set.insert n seen) ++
           filter (\(n2, _) -> not (Set.member n2 seen) && n2 /= n) ys

foldM :: (a -> b -> Either e a) -> a -> [b] -> Either e a
foldM _ acc []     = Right acc
foldM f acc (x:xs) = f acc x >>= \acc' -> foldM f acc' xs

freeVars :: InferState -> Ty -> Set.Set Int
freeVars s ty = collectFree (apply s ty)

freeVarsScheme :: InferState -> Scheme -> Set.Set Int
freeVarsScheme s sc =
  let tyFree = collectFree (apply s (schemeTy sc))
      bound  = Set.fromList (schemeVars sc)
  in tyFree `Set.difference` bound

generalize :: InferState -> Ty -> Scheme
generalize s ty0 =
  let ty = apply s ty0
      freeInTy = freeVars s ty
      freeInEnv = Set.unions (map (freeVarsScheme s . snd) (env s))
      vars = Set.toList (freeInTy `Set.difference` freeInEnv)
  in Scheme vars ty

instantiate :: InferState -> Scheme -> (Ty, InferState)
instantiate s0 sc =
  let (mapping, s1) = foldl (\(m, st) v ->
        let (fv, st') = fresh st
        in (Map.insert v (TyVar fv) m, st')
        ) (Map.empty, s0) (schemeVars sc)
  in (substScheme (schemeTy sc) mapping, s1)

lookupEnv :: String -> InferState -> Maybe Scheme
lookupEnv name s = fmap snd $ find (\(n, _) -> n == name) (reverse (env s))

pushEnv :: String -> Scheme -> InferState -> InferState
pushEnv name sc s = s { env = env s ++ [(name, sc)] }

truncateEnv :: Int -> InferState -> InferState
truncateEnv n s = s { env = take n (env s) }

typeExprToTy :: InferState -> TypeExpr -> Map.Map String Int -> Either String (Ty, InferState)
typeExprToTy s te params = case te of
  TEVar name -> case Map.lookup name params of
    Just i  -> Right (TyVar i, s)
    Nothing -> Left $ "Unknown type variable: " ++ name
  TENamed name args -> do
    (argTys, s1) <- mapAccum s args (\st a -> typeExprToTy st a params)
    case name of
      "Integer"   -> Right (TyInt, s1)
      "Double"    -> Right (TyDouble, s1)
      "Logical"   -> Right (TyBool, s1)
      "Character" -> Right (TyStr, s1)
      _ -> case Map.lookup name (typeAliases s1) of
        Just (aliasParams, aliasBody) -> do
          let (aliasMap, s2) = foldl (\(m, st) (pn, argTy) ->
                let (fv, st') = fresh st
                    st'' = st' { subst = Map.insert fv argTy (subst st') }
                in (Map.insert pn fv m, st'')
                ) (params, s1) (zip aliasParams argTys)
          (tys, s3) <- mapAccum s2 aliasBody (\st te' -> typeExprToTy st te' aliasMap)
          case tys of
            [single] -> Right (single, s3)
            _ -> let ret = last tys
                     ps  = init tys
                 in Right (TyFunc ps ret, s3)
        Nothing -> Right (TyAdt name argTys, s1)
  TEFunc paramExprs retExpr -> do
    (paramTys, s1) <- mapAccum s paramExprs (\st p -> typeExprToTy st p params)
    (retTy, s2) <- typeExprToTy s1 retExpr params
    Right (TyFunc paramTys retTy, s2)
  TERecord fields -> do
    (fieldTys, s1) <- mapAccum s fields (\st (n, te') -> do
      (ty, st') <- typeExprToTy st te' params
      Right ((n, ty), st'))
    let sorted = sortBy (comparing fst) fieldTys
    Right (TyRecord sorted, s1)
  TEDataframe fields -> do
    (fieldTys, s1) <- mapAccum s fields (\st (n, te') -> do
      (ty, st') <- typeExprToTy st te' params
      Right ((n, ty), st'))
    let sorted = sortBy (comparing fst) fieldTys
    Right (TyRecord sorted, s1)

mapAccum :: s -> [a] -> (s -> a -> Either e (b, s)) -> Either e ([b], s)
mapAccum s0 [] _ = Right ([], s0)
mapAccum s0 (x:xs) f = do
  (b, s1) <- f s0 x
  (bs, s2) <- mapAccum s1 xs f
  Right (b : bs, s2)

registerBuiltins :: InferState -> InferState
registerBuiltins s0 =
  let (a0, s1) = fresh s0
      (b0, s2) = fresh s1
      vecA0 = TyAdt "Vector" [TyVar a0]
      vecB0 = TyAdt "Vector" [TyVar b0]
      s3 = pushEnv "map" (Scheme [a0, b0] (TyFunc [TyFunc [TyVar a0] (TyVar b0), vecA0] vecB0)) s2

      (a1, s4) = fresh s3
      (b1, s5) = fresh s4
      (c1, s6) = fresh s5
      vecA1 = TyAdt "Vector" [TyVar a1]
      vecB1 = TyAdt "Vector" [TyVar b1]
      vecC1 = TyAdt "Vector" [TyVar c1]
      s7 = pushEnv "map2" (Scheme [a1, b1, c1]
        (TyFunc [TyFunc [TyVar a1, TyVar b1] (TyVar c1), vecA1, vecB1] vecC1)) s6

      (a2, s8)  = fresh s7
      (b2, s9)  = fresh s8
      vecA2 = TyAdt "Vector" [TyVar a2]
      s10 = pushEnv "reduce" (Scheme [a2, b2]
        (TyFunc [TyFunc [TyVar b2, TyVar a2] (TyVar b2), TyVar b2, vecA2] (TyVar b2))) s9

      (a3, s11) = fresh s10
      vecA3 = TyAdt "Vector" [TyVar a3]
      s12 = pushEnv "keep" (Scheme [a3]
        (TyFunc [TyFunc [TyVar a3] TyBool, vecA3] vecA3)) s11

      (a4, s13) = fresh s12
      vecA4 = TyAdt "Vector" [TyVar a4]
      s14 = pushEnv "discard" (Scheme [a4]
        (TyFunc [TyFunc [TyVar a4] TyBool, vecA4] vecA4)) s13
  in s14

registerType :: InferState -> TypeDecl -> Either String InferState
registerType s0 td = do
  let (paramMap, typeVars, s1) = foldl (\(m, vs, st) pn ->
        let (tv, st') = fresh st
        in (Map.insert pn tv m, vs ++ [tv], st')
        ) (Map.empty, [], s0) (typeParams td)
      resultTy = TyAdt (typeName td) (map TyVar typeVars)
  foldM (\st v -> do
    let ctorTy = if null (variantFields v)
          then resultTy
          else let fieldTysE = mapM (\f -> typeExprToTy st f paramMap) (variantFields v)
               in case fieldTysE of
                    Left e -> error e
                    Right pairs ->
                      let (fts, _) = unzip pairs
                      in TyFunc fts resultTy
    let scheme = Scheme typeVars ctorTy
    Right $ pushEnv (variantName v) scheme st
    ) s1 (typeVariants td)

registerExtern :: InferState -> ExternDecl -> Either String InferState
registerExtern s0 ed = do
  let (paramMap, s1) = collectTypeVarsFromExprs s0 (externTyExpr ed)
  (tys, s2) <- mapAccum s1 (externTyExpr ed) (\st te -> typeExprToTy st te paramMap)
  let vars = Map.elems paramMap
      ty = case tys of
        [single] -> single
        _ -> TyFunc (init tys) (last tys)
  Right $ pushEnv (externName ed) (Scheme vars ty) s2

collectTypeVarsFromExprs :: InferState -> [TypeExpr] -> (Map.Map String Int, InferState)
collectTypeVarsFromExprs s0 tes = foldl collectTV (Map.empty, s0) tes
  where
    collectTV (m, s) te = case te of
      TEVar name
        | Map.member name m -> (m, s)
        | otherwise -> let (fv, s') = fresh s in (Map.insert name fv m, s')
      TENamed _ args -> foldl collectTV (m, s) args
      TEFunc ps r -> let (m', s') = foldl collectTV (m, s) ps in collectTV (m', s') r
      TERecord fs -> foldl (\(m', s') (_, t) -> collectTV (m', s') t) (m, s) fs
      TEDataframe fs -> foldl (\(m', s') (_, t) -> collectTV (m', s') t) (m, s) fs

annotationToTy :: InferState -> [TypeExpr] -> Either String (Ty, InferState)
annotationToTy s0 parts = do
  let (paramMap, s1) = collectTypeVarsFromExprs s0 parts
  (tys, s2) <- mapAccum s1 parts (\st te -> typeExprToTy st te paramMap)
  case tys of
    [single] -> Right (single, s2)
    _ -> Right (TyFunc (init tys) (last tys), s2)

inferExpr :: InferState -> Expr -> Either String (Ty, InferState)
inferExpr s expr = case expr of
  EVar name -> case lookupEnv name s of
    Just sc -> let (ty, s') = instantiate s sc in Right (ty, s')
    Nothing -> Left $ "Unbound variable: " ++ name

  EInt _   -> Right (TyInt, s)
  EFloat _ -> Right (TyDouble, s)
  EBool _  -> Right (TyBool, s)
  EStr _   -> Right (TyStr, s)

  EVecLit elems -> do
    let (elemVar, s1) = fresh s
        elemTy = TyVar elemVar
    s2 <- foldM (\st e -> do
      (t, st') <- inferExpr st e
      unify st' elemTy t
      ) s1 elems
    Right (TyAdt "Vector" [apply s2 elemTy], s2)

  ELambda params body -> do
    let (paramTys, s1) = foldl (\(tys, st) _ ->
          let (fv, st') = fresh st in (tys ++ [TyVar fv], st')
          ) ([], s) params
        prev = length (env s1)
        s2 = foldl (\st (name, ty) -> pushEnv name (Scheme [] ty) st) s1 (zip params paramTys)
    (retTy, s3) <- inferExpr s2 body
    let s4 = truncateEnv prev s3
    Right (TyFunc paramTys retTy, s4)

  ECall func args -> do
    (funcTy, s1) <- inferExpr s func
    let realArgs = filter (not . isUnit) args
    if null realArgs
      then let (rv, s2) = fresh s1 in Right (TyVar rv, s2)
      else do
        (argTys, s2) <- mapAccum s1 realArgs (\st a -> inferExpr st a)
        let (retVar, s3) = fresh s2
            expected = TyFunc argTys (TyVar retVar)
        s4 <- unify s3 funcTy expected
        Right (TyVar retVar, s4)

  EIf cond thenE elseE -> do
    (ct, s1) <- inferExpr s cond
    s2 <- unify s1 ct TyBool
    (tt, s3) <- inferExpr s2 thenE
    (et, s4) <- inferExpr s3 elseE
    s5 <- unify s4 tt et
    Right (tt, s5)

  EMatch scrutinee arms -> do
    (scrutTy, s1) <- inferExpr s scrutinee
    let (resultVar, s2) = fresh s1
        resultTy = TyVar resultVar
    s3 <- foldM (\st (pat, body) -> do
      let prev = length (env st)
      st1 <- checkPattern st pat scrutTy
      (bodyTy, st2) <- inferExpr st1 body
      st3 <- unify st2 resultTy bodyTy
      Right $ truncateEnv prev st3
      ) s2 arms
    Right (resultTy, s3)

  EBinary op left right -> do
    (lt, s1) <- inferExpr s left
    (rt, s2) <- inferExpr s1 right
    case op of
      Add -> do s3 <- unify s2 lt rt; Right (apply s3 lt, s3)
      Sub -> do s3 <- unify s2 lt rt; Right (apply s3 lt, s3)
      Mul -> do s3 <- unify s2 lt rt; Right (apply s3 lt, s3)
      Div -> do
        s3 <- unify s2 lt TyDouble
        s4 <- unify s3 rt TyDouble
        Right (TyDouble, s4)
      _ -> do
        s3 <- unify s2 lt rt
        Right (TyBool, s3)

  ELet bindings body -> do
    let prev = length (env s)
    s1 <- foldM (\st (name, bindExpr) -> do
      (ty, st1) <- inferExpr st bindExpr
      if name == "_" || '.' `elem` name
        then Right st1
        else let ty' = apply st1 ty
                 sc  = generalize st1 ty'
             in Right $ pushEnv name sc st1
      ) s bindings
    (result, s2) <- inferExpr s1 body
    Right (result, truncateEnv prev s2)

  EUnit -> let (v, s') = fresh s in Right (TyVar v, s')

  EDplyr _ _ -> let (v, s') = fresh s in Right (TyVar v, s')

  ERecord fields -> do
    (fieldTys, s1) <- mapAccum s fields (\st (n, e) -> do
      (ty, st') <- inferExpr st e
      Right ((n, ty), st'))
    let sorted = sortBy (comparing fst) fieldTys
    Right (TyRecord sorted, s1)

  EDataFrame fields -> do
    (fieldTys, s1) <- mapAccum s fields (\st (n, e) -> do
      (ty, st') <- inferExpr st e
      Right ((n, ty), st'))
    let sorted = sortBy (comparing fst) fieldTys
    Right (TyRecord sorted, s1)

  EFieldAccess fieldExpr field -> do
    (rawTy, s1) <- inferExpr s fieldExpr
    let resolved = apply s1 rawTy
    case resolved of
      TyRecord fields -> case lookup field fields of
        Just ty -> Right (ty, s1)
        Nothing -> case rawTy of
          TyVar i -> do
            let (fv, s2) = fresh s1
                fieldTy = TyVar fv
                newFields = sortBy (comparing fst) (fields ++ [(field, fieldTy)])
            Right (fieldTy, s2 { subst = Map.insert i (TyRecord newFields) (subst s2) })
          _ -> Left $ "Record has no field '" ++ field ++ "'"
      TyVar i -> do
        let (fv, s2) = fresh s1
            fieldTy = TyVar fv
            rec = TyRecord [(field, fieldTy)]
        Right (fieldTy, s2 { subst = Map.insert i rec (subst s2) })
      _ -> Left $ "Cannot access field '" ++ field ++ "' on non-record type"

  EPipe left right -> case right of
    ECall f args -> inferExpr s (ECall f (args ++ [left]))
    EVar _       -> inferExpr s (ECall right [left])
    EDplyr verb dArgs -> do
      (leftTy, s1) <- inferExpr s left
      let resolved = apply s1 leftTy
      case resolved of
        TyRecord fields -> do
          s2 <- checkDplyrStep s1 verb dArgs fields
          let resultTy = evolveDplyrSchema s2 verb dArgs fields
          Right (resultTy, s2)
        _ -> inferExpr s1 right
    _ -> Left "Right side of |> must be a function or call"

isUnit :: Expr -> Bool
isUnit EUnit = True
isUnit _     = False

checkPattern :: InferState -> Pattern -> Ty -> Either String InferState
checkPattern s pat expected = case pat of
  PWildcard -> Right s
  PVar name ->
    let resolved = apply s expected
    in Right $ pushEnv name (Scheme [] resolved) s
  PInt _ -> unify s expected TyInt
  PBool _ -> unify s expected TyBool
  PConstructor name pats -> do
    sc <- case lookupEnv name s of
      Just sc' -> Right sc'
      Nothing  -> Left $ "Unknown constructor: " ++ name
    let (ctorTy, s1) = instantiate s sc
    if null pats
      then unify s1 ctorTy expected
      else do
        let ctorApplied = apply s1 ctorTy
        case ctorApplied of
          TyFunc fieldTys resultTy
            | length fieldTys /= length pats ->
              Left $ "Constructor " ++ name ++ " expects " ++ show (length fieldTys) ++ " fields, got " ++ show (length pats)
            | otherwise -> do
              s2 <- unify s1 resultTy expected
              foldM (\st (p, ft) -> checkPattern st p ft) s2 (zip pats fieldTys)
          _ -> Left $ "Constructor " ++ name ++ " is not a function type"

checkDplyrStep :: InferState -> String -> [DplyrArg] -> [(String, Ty)] -> Either String InferState
checkDplyrStep s0 verb args schema = do
  let colNames = map fst schema
  foldM (\st arg -> case arg of
    DplyrColumn name
      | name `elem` colNames -> Right st
      | otherwise -> Left $ "Unknown column '" ++ name ++ "' in " ++ verb ++
                     ". Available columns: " ++ intercalate ", " colNames
    DplyrPred dExpr  -> inferDplyrColExpr st dExpr schema verb >> Right st
    DplyrNamed _ dExpr -> inferDplyrColExpr st dExpr schema verb >> Right st
    ) s0 args

inferDplyrColExpr :: InferState -> Expr -> [(String, Ty)] -> String -> Either String Ty
inferDplyrColExpr s expr schema verb = case expr of
  EVar name
    | Just (_, ty) <- find (\(n,_) -> n == name) schema -> Right (unwrapVector ty)
    | Just sc <- lookupEnv name s ->
      let (ty, _) = instantiate s sc
          resolved = apply s ty
      in case resolved of
        TyFunc{} -> Right resolved
        _ -> Left $ "Unknown column '" ++ name ++ "' in " ++ verb ++
              ". Available columns: " ++ intercalate ", " (map fst schema)
    | otherwise -> Left $ "Unknown column '" ++ name ++ "' in " ++ verb ++
                   ". Available columns: " ++ intercalate ", " (map fst schema)
  EBinary op l r -> do
    lt <- inferDplyrColExpr s l schema verb
    rt <- inferDplyrColExpr s r schema verb
    let lName = exprName l
        rName = exprName r
        opSym = binOpSymbol op
        numErr side name' = "Type mismatch in " ++ verb ++ ": '" ++ name' ++ "' is Character, cannot use '" ++ opSym ++ "'"
    case op of
      Div -> do
        case lt of
          TyStr -> Left $ numErr "left" lName
          _ -> Right ()
        case rt of
          TyStr -> Left $ numErr "right" rName
          _ -> Right ()
        Right TyDouble
      Eq -> Right TyBool; Neq -> Right TyBool
      Gt -> Right TyBool; Lt  -> Right TyBool
      GtEq -> Right TyBool; LtEq -> Right TyBool
      Add -> do
        case (lt, rt) of
          (TyStr, _) -> Left $ numErr "left" lName
          (_, TyStr) -> Left $ numErr "right" rName
          _ -> Right lt
      Sub -> do
        case (lt, rt) of
          (TyStr, _) -> Left $ numErr "left" lName
          (_, TyStr) -> Left $ numErr "right" rName
          _ -> Right lt
      Mul -> do
        case (lt, rt) of
          (TyStr, _) -> Left $ numErr "left" lName
          (_, TyStr) -> Left $ numErr "right" rName
          _ -> Right lt
  ECall func args -> do
    let realArgs = filter (not . isUnit) args
    case func of
      EVar fname -> case lookupEnv fname s of
        Just sc ->
          let (ty, s') = instantiate s sc
              resolved = apply s' ty
          in case resolved of
            TyFunc _ retTy -> Right (apply s' retTy)
            _ -> Right resolved
        Nothing -> case realArgs of
          [a] -> inferDplyrColExpr s a schema verb
          _   -> Right TyDouble
      _ -> Right TyDouble
  EInt _   -> Right TyInt
  EFloat _ -> Right TyDouble
  EBool _  -> Right TyBool
  EStr _   -> Right TyStr
  _ -> Right TyDouble

exprName :: Expr -> String
exprName (EVar n) = n
exprName (EInt n) = show n
exprName (EFloat f) = show f
exprName (EBool b) = if b then "True" else "False"
exprName (EStr s) = "\"" ++ s ++ "\""
exprName _ = "<expr>"

unwrapVector :: Ty -> Ty
unwrapVector (TyAdt "Vector" [elem']) = elem'
unwrapVector other = other

evolveDplyrSchema :: InferState -> String -> [DplyrArg] -> [(String, Ty)] -> Ty
evolveDplyrSchema s verb args schema = case verb of
  "select" ->
    let selected = [n | DplyrColumn n <- args]
    in if null selected
       then TyRecord schema
       else TyRecord (sortBy (comparing fst) (filter (\(n, _) -> n `elem` selected) schema))
  "group_by" -> TyRecord schema
  "arrange"  -> TyRecord schema
  "filter"   -> TyRecord schema
  "mutate" ->
    let newFields = foldl (\fs arg -> case arg of
          DplyrNamed name dExpr ->
            let colTy = case inferDplyrColExpr s dExpr schema verb of
                  Right ty -> TyAdt "Vector" [ty]
                  Left _   -> TyAdt "Vector" [TyDouble]
            in replaceOrAdd name colTy fs
          _ -> fs
          ) schema args
    in TyRecord (sortBy (comparing fst) newFields)
  "summarize" ->
    let fields = [(n, colTy dExpr) | DplyrNamed n dExpr <- args]
        colTy dExpr = case inferDplyrColExpr s dExpr schema verb of
          Right ty -> TyAdt "Vector" [ty]
          Left _   -> TyAdt "Vector" [TyDouble]
    in TyRecord (sortBy (comparing fst) fields)
  _ -> TyRecord schema
  where
    replaceOrAdd name ty fs =
      if any (\(n, _) -> n == name) fs
      then map (\(n, t) -> if n == name then (n, ty) else (n, t)) fs
      else fs ++ [(name, ty)]

inferProgram :: InferState -> Program -> Either String ([(String, Ty)], InferState)
inferProgram s0 prog = do
  let s1 = registerBuiltins s0
  s2 <- foldM (\st d -> case d of
    DeclType td -> registerType st td
    DeclTypeAlias ta ->
      Right st { typeAliases = Map.insert (aliasName ta) (aliasParams ta, aliasBody ta) (typeAliases st) }
    DeclImport ed -> registerExtern st ed
    _ -> Right st
    ) s1 (progDecls prog)
  (types, s3) <- foldM (\(tys, st) d -> case d of
    DeclLet ld -> do
      let (preVar, st1) = fresh st
          st2 = pushEnv (letName ld) (Scheme [] (TyVar preVar)) st1
          wrapErr e = Left $ "In definition of '" ++ letName ld ++ "': " ++ e
      (ty, st3) <- case inferExpr st2 (letExpr ld) of
        Right r -> Right r
        Left e  -> wrapErr e
      st4 <- case unify st3 (TyVar preVar) ty of
        Right r -> Right r
        Left e  -> wrapErr e
      st5 <- case letAnnotation ld of
        Just ann -> do
          (annTy, st4') <- annotationToTy st4 ann
          case unify st4' ty annTy of
            Right r -> Right r
            Left e  -> wrapErr e
        Nothing -> Right st4
      let ty' = apply st5 ty
          st6 = st5 { env = init (env st5) }
          sc  = generalize st6 ty'
          st7 = pushEnv (letName ld) sc st6
      Right (tys ++ [(letName ld, ty')], st7)
    _ -> Right (tys, st)
    ) ([], s2) (progDecls prog)
  let finalTypes = map (\(n, t) -> (n, apply s3 t)) types
  Right (finalTypes, s3)

runInfer :: Program -> Either String ([(String, Ty)], InferState)
runInfer = inferProgram newInferState
