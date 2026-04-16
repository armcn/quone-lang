module Quone.AST.Source
  ( Program(..)
  , ModuleDecl(..)
  , Exports(..)
  , Decl(..)
  , TypeDecl(..)
  , VariantDecl(..)
  , TypeAliasDecl(..)
  , TypeExpr(..)
  , LetDecl(..)
  , ExternDecl(..)
  , Expr(..)
  , DplyrArg(..)
  , BinOp(..)
  , Pattern(..)
  , binOpPrecedence
  , binOpSymbol
  ) where

data Program = Program
  { progModule :: Maybe ModuleDecl
  , progDecls  :: [Decl]
  } deriving (Show)

data ModuleDecl = ModuleDecl
  { moduleName    :: String
  , moduleExports :: Exports
  } deriving (Show)

data Exports
  = ExportAll
  | ExportList [String]
  deriving (Show)

data Decl
  = DeclType TypeDecl
  | DeclTypeAlias TypeAliasDecl
  | DeclLet LetDecl
  | DeclImport ExternDecl
  | DeclLibrary String
  deriving (Show)

data TypeDecl = TypeDecl
  { typeName     :: String
  , typeParams   :: [String]
  , typeVariants :: [VariantDecl]
  } deriving (Show)

data VariantDecl = VariantDecl
  { variantName   :: String
  , variantFields :: [TypeExpr]
  } deriving (Show)

data TypeAliasDecl = TypeAliasDecl
  { aliasName   :: String
  , aliasParams :: [String]
  , aliasBody   :: [TypeExpr]
  } deriving (Show)

data TypeExpr
  = TEVar String
  | TENamed String [TypeExpr]
  | TEFunc [TypeExpr] TypeExpr
  | TERecord [(String, TypeExpr)]
  | TEDataframe [(String, TypeExpr)]
  deriving (Show)

data LetDecl = LetDecl
  { letName       :: String
  , letExpr       :: Expr
  , letAnnotation :: Maybe [TypeExpr]
  } deriving (Show)

data ExternDecl = ExternDecl
  { externName   :: String
  , externTyExpr :: [TypeExpr]
  } deriving (Show)

data Expr
  = EVar String
  | EInt Integer
  | EFloat Double
  | EBool Bool
  | EStr String
  | EVecLit [Expr]
  | ELambda [String] Expr
  | ECall Expr [Expr]
  | EIf Expr Expr Expr
  | EMatch Expr [(Pattern, Expr)]
  | EBinary BinOp Expr Expr
  | ELet [(String, Expr)] Expr
  | EPipe Expr Expr
  | ERecord [(String, Expr)]
  | EDataFrame [(String, Expr)]
  | EFieldAccess Expr String
  | EUnit
  | EDplyr String [DplyrArg]
  deriving (Show)

data DplyrArg
  = DplyrColumn String
  | DplyrPred Expr
  | DplyrNamed String Expr
  deriving (Show)

data BinOp
  = Add | Sub | Mul | Div
  | Eq | Neq | Gt | Lt | GtEq | LtEq
  deriving (Show, Eq)

data Pattern
  = PWildcard
  | PVar String
  | PInt Integer
  | PBool Bool
  | PConstructor String [Pattern]
  deriving (Show)

binOpPrecedence :: BinOp -> Int
binOpPrecedence op = case op of
  Eq   -> 1; Neq  -> 1; Gt -> 1; Lt -> 1; GtEq -> 1; LtEq -> 1
  Add  -> 2; Sub  -> 2
  Mul  -> 3; Div  -> 3

binOpSymbol :: BinOp -> String
binOpSymbol op = case op of
  Add  -> "+";  Sub  -> "-"
  Mul  -> "*";  Div  -> "/"
  Eq   -> "=="; Neq  -> "!="
  Gt   -> ">";  Lt   -> "<"
  GtEq -> ">="; LtEq -> "<="
