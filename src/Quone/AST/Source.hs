{-|
Module: Quone.AST.Source

The abstract syntax tree of a Quone source file.

A 'Program' is the top of the tree. It contains an optional package
declaration and a list of top-level declarations: type declarations,
type aliases, value bindings (@let@-style), imports, and library
references.

Inside value bindings live 'Expr' nodes — the meat of the language.
Expressions can be variables, literals, lambdas, calls, pattern matches,
and so on.

Each constructor is documented in the order it appears in the file.
-}
module Quone.AST.Source
    ( -- The program tree
      Program(..)
    , ModuleDecl(..)
    , Exports(..)

      -- Top-level declarations
    , Decl(..)
    , TypeDecl(..)
    , VariantDecl(..)
    , TypeAliasDecl(..)
    , LetDecl(..)
    , ExternDecl(..)

      -- Type expressions (the right-hand side of a type signature)
    , TypeExpr(..)

      -- Expressions (the right-hand side of a value binding)
    , Expr(..)
    , DplyrArg(..)
    , BinOp(..)
    , Pattern(..)

      -- Helpers for binary operators
    , binOpPrecedence
    , binOpSymbol
    ) where

import Quone.Prelude


-- | A whole Quone source file.
data Program = Program
    { progModule :: Maybe ModuleDecl
    , progDecls :: List Decl
    }
    deriving (Show)


-- | The optional @package Foo exporting (..)@ header at the top of a file.
data ModuleDecl = ModuleDecl
    { moduleName :: String
    , moduleExports :: Exports
    }
    deriving (Show)


-- | What a package exports.
data Exports
    = ExportAll
    | ExportList (List String)
    deriving (Show)


-- | A top-level declaration. There are five kinds.
data Decl
    = DeclType TypeDecl
    | DeclTypeAlias TypeAliasDecl
    | DeclLet LetDecl
    | DeclImport ExternDecl
    | DeclLibrary String
    deriving (Show)


-- | A custom type with one or more variants:
--
-- > type Option a
-- >     <- None
-- >     | Some a
data TypeDecl = TypeDecl
    { typeName :: String
    , typeParams :: List String
    , typeVariants :: List VariantDecl
    }
    deriving (Show)


-- | One variant of a custom type. @None@ has no fields, @Some a@ has one.
data VariantDecl = VariantDecl
    { variantName :: String
    , variantFields :: List TypeExpr
    }
    deriving (Show)


-- | A type alias:
--
-- > type alias Students <-
-- >     dataframe { name : Vector Character, score : Vector Double }
data TypeAliasDecl = TypeAliasDecl
    { aliasName :: String
    , aliasParams :: List String
    , aliasBody :: List TypeExpr
    }
    deriving (Show)


-- | A type expression — what you write on the right of a @:@ in a signature
-- or in a type alias body.
data TypeExpr
    = TEVar String
    | TENamed String (List TypeExpr)
    | TEFunc (List TypeExpr) TypeExpr
    | TERecord (List ( String, TypeExpr ))
    | TEDataframe (List ( String, TypeExpr ))
    deriving (Show)


-- | A value binding:
--
-- > rmse predicted actual <-
-- >     ...
--
-- The optional annotation is the type signature line above it.
data LetDecl = LetDecl
    { letName :: String
    , letExpr :: Expr
    , letAnnotation :: Maybe (List TypeExpr)
    }
    deriving (Show)


-- | An imported function from R:
--
-- > import sqrt : Double -> Double
data ExternDecl = ExternDecl
    { externName :: String
    , externTyExpr :: List TypeExpr
    }
    deriving (Show)


-- | An expression — the right-hand side of a binding, or anything that
-- produces a value.
data Expr
    = EVar String
    | EInt Integer
    | EFloat Double
    | EBool Bool
    | EStr String
    | EVecLit (List Expr)
    | ELambda (List String) Expr
    | ECall Expr (List Expr)
    | EIf Expr Expr Expr
    | EMatch Expr (List ( Pattern, Expr ))
    | EBinary BinOp Expr Expr
    | ELet (List ( String, Expr )) Expr
    | EPipe Expr Expr
    | ERecord (List ( String, Expr ))
    | EDataFrame (List ( String, Expr ))
    | EFieldAccess Expr String
    | EUnit
    | EDplyr String (List DplyrArg)
    deriving (Show)


-- | A single argument to a dplyr verb (@select@, @filter@, @mutate@, ...).
data DplyrArg
    = DplyrColumn String
    | DplyrPred Expr
    | DplyrNamed String Expr
    deriving (Show)


-- | The binary operators recognised by the parser.
data BinOp
    = Add
    | Sub
    | Mul
    | Div
    | Eq
    | Neq
    | Gt
    | Lt
    | GtEq
    | LtEq
    deriving (Show, Eq)


-- | A pattern as it appears in a @case ... of@.
data Pattern
    = PWildcard
    | PVar String
    | PInt Integer
    | PBool Bool
    | PConstructor String (List Pattern)
    deriving (Show)


-- | Higher numbers mean tighter binding. Used by both the parser
-- (for precedence climbing) and the code generator (to decide when
-- to add parentheses).
binOpPrecedence :: BinOp -> Int
binOpPrecedence op =
    case op of
        Eq ->
            1

        Neq ->
            1

        Gt ->
            1

        Lt ->
            1

        GtEq ->
            1

        LtEq ->
            1

        Add ->
            2

        Sub ->
            2

        Mul ->
            3

        Div ->
            3


-- | The symbol the operator prints as.
binOpSymbol :: BinOp -> String
binOpSymbol op =
    case op of
        Add ->
            "+"

        Sub ->
            "-"

        Mul ->
            "*"

        Div ->
            "/"

        Eq ->
            "=="

        Neq ->
            "!="

        Gt ->
            ">"

        Lt ->
            "<"

        GtEq ->
            ">="

        LtEq ->
            "<="
