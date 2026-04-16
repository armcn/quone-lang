module Quone.Parse.Token
  ( Token(..)
  , Span(..)
  ) where

data Token
  = TType
  | TAlias
  | TImport
  | TDataframe
  | TLibrary
  | TPackage
  | TExporting
  | TDotDot
  | TLet
  | TIn
  | TCase
  | TOf
  | TIf
  | TThen
  | TElse
  | TTrue
  | TFalse
  | TBind       -- <-
  | TEquals     -- =
  | TEqEq       -- ==
  | TNeq        -- !=
  | TGt         -- >
  | TLt         -- <
  | TGtEq       -- >=
  | TLtEq       -- <=
  | TSelect
  | TFilter
  | TMutate
  | TSummarize
  | TGroupBy
  | TArrange
  | TPipe       -- |
  | TPipeRight  -- |>
  | TArrow      -- ->
  | TPlus
  | TMinus
  | TStar
  | TSlash
  | TBackslash
  | TColon
  | TLParen
  | TRParen
  | TLBrace
  | TRBrace
  | TLBracket
  | TRBracket
  | TComma
  | TDot
  | TUnderscore
  | TIdent String
  | TIntLit Integer
  | TFloatLit Double
  | TStringLit String
  | TEof
  deriving (Show, Eq)

data Span = Span
  { spanLine :: !Int
  , spanCol  :: !Int
  } deriving (Show, Eq)
