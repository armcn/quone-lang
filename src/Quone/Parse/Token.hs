{-|
Module: Quone.Parse.Token

The little tokens that the lexer produces and the parser consumes,
plus a 'Span' type that records where each token came from in the
source.

There's nothing tricky in this file — it's just two data types.
-}
module Quone.Parse.Token
    ( Token(..)
    , Span(..)
    ) where

import Quone.Prelude


-- | One token, as produced by 'Quone.Parse.Lexer.tokenize'.
data Token
    = -- Keywords
      TType
    | TAlias
    | TImport
    | TDataframe
    | TLibrary
    | TPackage
    | TExporting
    | TLet
    | TIn
    | TCase
    | TOf
    | TIf
    | TThen
    | TElse
    | TTrue
    | TFalse
    | TSelect
    | TFilter
    | TMutate
    | TSummarize
    | TGroupBy
    | TArrange
      -- Symbols and operators
    | TBind -- @<-@
    | TEquals -- @=@
    | TEqEq -- @==@
    | TNeq -- @!=@
    | TGt -- @>@
    | TLt -- @<@
    | TGtEq -- @>=@
    | TLtEq -- @<=@
    | TPipe -- @|@
    | TPipeRight -- @|>@
    | TArrow -- @->@
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
    | TDotDot
    | TUnderscore
      -- Things with a payload
    | TIdent String
    | TIntLit Integer
    | TFloatLit Double
    | TStringLit String
      -- The end of input
    | TEof
    deriving (Show, Eq)


-- | The line and column where a token was found. Used for error messages
-- and the LSP server.
data Span = Span
    { spanLine :: !Int
    , spanCol :: !Int
    }
    deriving (Show, Eq)
