module Quone
  ( compile
  , compileToR
  , check
  , formatSource
  , generateNamespace
  , generateDescription
  , CompileResult(..)
  , ModuleInfo(..)
  , CompileError(..)
  ) where

import Data.List (intercalate, isPrefixOf, isInfixOf, findIndex)

import Quone.AST.Source
import Quone.Parse.Lexer (tokenize)
import Quone.Parse.Parser (parseProgram)
import Quone.Type.Type ()
import Quone.Type.Infer (InferState(..), inferProgram)
import Quone.Generate.R (generate)
import qualified Quone.Format as Fmt

data CompileResult = CompileResult
  { crRCode  :: String
  , crModule :: Maybe ModuleInfo
  }

data ModuleInfo = ModuleInfo
  { miName    :: String
  , miExports :: [String]
  }

data CompileError = CompileError
  { ceMessage :: String
  , ceLine    :: Int
  , ceCol     :: Int
  } deriving (Show)

prelude :: String
prelude = unlines
  [ "import sqrt : Double -> Double"
  , "import mean : Vector a -> Double"
  , "import sum : Vector a -> Double"
  , "import length : Vector a -> Integer"
  , "import to_double : Integer -> Double"
  ]

injectPrelude :: String -> String
injectPrelude source
  | "package " `isPrefixOf` stripped =
    case findIndex (== '\n') source of
      Just pos -> take (pos + 1) source ++ prelude ++ drop (pos + 1) source
      Nothing  -> source ++ "\n" ++ prelude
  | otherwise = prelude ++ source
  where stripped = dropWhile (== ' ') source

compile :: String -> Either String CompileResult
compile source = do
  let fullSource = injectPrelude source
  (toks, sps) <- tokenize fullSource
  prog <- parseProgram toks sps
  let s0 = InferState 0 mempty [] mempty
  (types, inferSt) <- inferProgram s0 prog
  let rCode = generate prog types inferSt
      modInfo = fmap toModuleInfo (progModule prog)
  Right $ CompileResult rCode modInfo
  where
    toModuleInfo m = ModuleInfo
      { miName = moduleName m
      , miExports = resolveExports m
      }
    resolveExports m = case moduleExports m of
      ExportAll -> error "ExportAll requires program context"
      ExportList ns -> ns

compileToR :: String -> Either String String
compileToR source = crRCode <$> compile source

check :: String -> [CompileError]
check source =
  let isPackage = "package " `isPrefixOf` dropWhile (== ' ') source
      preludeLineCount = length (lines prelude)
      lineOffset = if isPackage then 0 else preludeLineCount
      fullSource = injectPrelude source
  in case pipeline fullSource of
    Right _  -> []
    Left msg -> [parseErrorWithFallback msg source lineOffset]
  where
    pipeline src = do
      (toks, sps) <- tokenize src
      prog <- parseProgram toks sps
      let s0 = InferState 0 mempty [] mempty
      _ <- inferProgram s0 prog
      Right ()

parseErrorWithFallback :: String -> String -> Int -> CompileError
parseErrorWithFallback msg source lineOffset =
  case parseLocatedError msg of
    Just (line, col, message) ->
      let adjusted = if line > lineOffset then line - lineOffset else 1
      in CompileError message adjusted col
    Nothing ->
      case findErrorLocation msg source of
        Just (line, col) -> CompileError msg line col
        Nothing -> CompileError msg 1 1

parseLocatedError :: String -> Maybe (Int, Int, String)
parseLocatedError msg =
  case break (== ':') msg of
    (lineStr, ':':rest) ->
      case break (== ':') rest of
        (colStr, ':':message) ->
          case (reads lineStr, reads colStr) of
            ([(line, "")], [(col, "")]) -> Just (line, col, message)
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing

findErrorLocation :: String -> String -> Maybe (Int, Int)
findErrorLocation msg source =
  let patterns =
        [ ("Unknown column '", "'")
        , ("Unknown function or column '", "'")
        , ("Unbound variable: ", "")
        , ("Record has no field '", "'")
        , ("Cannot access field '", "'")
        ]
      srcLines = zip [1..] (lines source)
  in tryPatterns patterns msg srcLines

tryPatterns :: [(String, String)] -> String -> [(Int, String)] -> Maybe (Int, Int)
tryPatterns [] msg srcLines = tryKeywords msg srcLines
tryPatterns ((prefix, suffix):rest) msg srcLines =
  case extractName prefix suffix msg of
    Just name -> findNameInLines name srcLines
    Nothing -> tryPatterns rest msg srcLines

tryKeywords :: String -> [(Int, String)] -> Maybe (Int, Int)
tryKeywords msg srcLines =
  let keywords = ["mutate", "filter", "select", "summarize", "arrange", "group_by"]
  in go keywords
  where
    go [] = Nothing
    go (kw:kws)
      | kw `isInfixOf` msg = findNameInLines kw srcLines
      | otherwise = go kws

extractName :: String -> String -> String -> Maybe String
extractName prefix suffix msg =
  case findSubstring prefix msg of
    Nothing -> Nothing
    Just idx ->
      let rest = drop (idx + length prefix) msg
          name = if null suffix
                 then takeWhile (\c -> c /= ' ' && c /= '\n') rest
                 else takeWhile (/= head suffix) rest
      in if null name then Nothing else Just name

findSubstring :: String -> String -> Maybe Int
findSubstring _ [] = Nothing
findSubstring needle haystack = go 0 haystack
  where
    go _ [] = Nothing
    go i s
      | needle `isPrefixOf` s = Just i
      | otherwise = go (i + 1) (tail s)

findNameInLines :: String -> [(Int, String)] -> Maybe (Int, Int)
findNameInLines _ [] = Nothing
findNameInLines name ((lineNum, lineText):rest) =
  case findSubstring name lineText of
    Just col -> Just (lineNum, col + 1)
    Nothing  -> findNameInLines name rest

formatSource :: String -> Either String String
formatSource = Fmt.formatSource

generateNamespace :: ModuleInfo -> String
generateNamespace info =
  "# Generated by quone\n\n"
  ++ concatMap (\n -> "export(" ++ n ++ ")\n") (miExports info)

generateDescription :: ModuleInfo -> String
generateDescription info = intercalate "\n"
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
