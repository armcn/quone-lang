{-|
Module: Main (the @quone@ executable)

The CLI entry point. Three modes:

  - @quone <file.Q>@         compile to R and print
  - @quone <file.Q> --format@ format the source and print
  - @quone <file.Q> --package@ generate an R package directory
  - @quone repl@             launch the interactive REPL

Most of the heavy lifting is in the 'Quone' library. This module
coordinates argument parsing, file IO, and (for the REPL) the
subprocess that runs R.
-}
module Main where

import Control.Exception (SomeException, try)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import System.Console.Haskeline
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.FilePath (takeBaseName, (</>))
import System.IO
    ( BufferMode(..)
    , Handle
    , hFlush
    , hGetLine
    , hPutStrLn
    , hReady
    , hSetBuffering
    )
import System.Process
    ( ProcessHandle
    , StdStream(..)
    , createProcess
    , proc
    , std_err
    , std_in
    , std_out
    )

import Quone



-- ====================================================================
-- ENTRY POINT
-- ====================================================================


main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName
    case args of
        [] ->
            printUsage progName

        [ "repl" ] ->
            runRepl

        input : flags ->
            runCompile input flags


printUsage :: String -> IO ()
printUsage progName = do
    putStrLn ("Usage: " ++ progName ++ " <input.Q> [--package | --format]")
    putStrLn ("       " ++ progName ++ " repl")
    exitFailure



-- ====================================================================
-- COMPILE MODE
-- ====================================================================


runCompile :: FilePath -> [String] -> IO ()
runCompile input flags = do
    let
        packageMode =
            "--package" `elem` flags

        formatMode =
            "--format" `elem` flags

    source <- readFile input

    if formatMode then
        runFormat source
    else do
        reportErrors input source
        result <- runOrDie input source
        if packageMode then
            runPackage input result
        else
            putStr (crRCode result)


runFormat :: String -> IO ()
runFormat source =
    case formatSource source of
        Right formatted ->
            putStr formatted

        Left e -> do
            putStrLn ("Format error: " ++ e)
            exitFailure


reportErrors :: FilePath -> String -> IO ()
reportErrors input source =
    case check source of
        e : _ -> do
            putStrLn
                ( input
                    ++ ":"
                    ++ show (ceLine e)
                    ++ ":"
                    ++ show (ceCol e)
                    ++ ": "
                    ++ ceMessage e
                )
            exitFailure

        [] ->
            return ()


runOrDie :: FilePath -> String -> IO CompileResult
runOrDie input source =
    case compile source of
        Right r ->
            return r

        Left e -> do
            putStrLn (input ++ ": " ++ e)
            exitFailure


runPackage :: FilePath -> CompileResult -> IO ()
runPackage input result =
    case crModule result of
        Nothing -> do
            putStrLn "Error: --package requires a package declaration"
            putStrLn "  Add: package MyPackage exporting (..)"
            exitFailure

        Just info -> do
            let
                pkgDir =
                    miName info

                rDir =
                    pkgDir </> "R"

                stem =
                    takeBaseName input

            createDirectoryIfMissing True rDir
            writeFile (rDir </> stem ++ ".R") (crRCode result)
            writeFile (pkgDir </> "NAMESPACE") (generateNamespace info)
            writeFile (pkgDir </> "DESCRIPTION") (generateDescription info)
            putStrLn ("Package created: " ++ pkgDir ++ "/")
            putStrLn ("  " ++ pkgDir ++ "/DESCRIPTION")
            putStrLn ("  " ++ pkgDir ++ "/NAMESPACE")
            putStrLn ("  " ++ pkgDir ++ "/R/" ++ stem ++ ".R")



-- ====================================================================
-- REPL
-- ====================================================================


runRepl :: IO ()
runRepl = do
    putStrLn (dim "Quone REPL — type :q to quit, :t <expr> to show type")
    putStrLn (dim "Shortcuts: Alt+- inserts <-  Alt+m inserts |>")
    prefs <- loadPrefs
    rProc <- startR
    case rProc of
        Nothing -> do
            putStrLn (red "Error: could not start R. Make sure R is installed.")
            exitFailure

        Just ( rIn, rOut, _ph ) -> do
            hSetBuffering rIn LineBuffering
            hSetBuffering rOut LineBuffering
            drainInitial rOut
            let
                settings =
                    defaultSettings { historyFile = Just ".quone_history" }
            runInputTWithPrefs prefs settings (loop newReplState rIn rOut)


-- | Write a haskeline preferences file with our keyboard shortcuts and
-- read it back.
loadPrefs :: IO Prefs
loadPrefs = do
    tmpDir <- getTemporaryDirectory
    let
        prefsFile =
            tmpDir </> "quone-haskeline"
    writeFile prefsFile $
        unlines
            [ "bind: meta-- < -"
            , "bind: meta-m | >"
            ]
    readPrefs prefsFile


-- | Eat any startup messages R might print before becoming responsive.
drainInitial :: Handle -> IO ()
drainInitial h = do
    ready <- hReady h
    when ready $ do
        _ <- hGetLine h
        drainInitial h


-- | Start an R subprocess with stdin/stdout pipes; let stderr go to the
-- terminal directly so we never block on a full pipe.
startR :: IO (Maybe ( Handle, Handle, ProcessHandle ))
startR = do
    let
        cp =
            (proc "R" [ "--no-save", "--quiet", "--no-echo" ])
                { std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = Inherit
                }
    result <- try (createProcess cp) :: IO (Either SomeException
                                                   ( Maybe Handle
                                                   , Maybe Handle
                                                   , Maybe Handle
                                                   , ProcessHandle
                                                   ))
    case result of
        Right ( Just hin, Just hout, _, ph ) ->
            return (Just ( hin, hout, ph ))

        _ ->
            return Nothing



-- --------------------------------------------------------------------
-- THE REPL LOOP
-- --------------------------------------------------------------------


loop :: ReplState -> Handle -> Handle -> InputT IO ()
loop rs rIn rOut = do
    minput <- getInputLine (bold "quone" ++ dim "> ")
    case minput of
        Nothing ->
            liftIO (putStrLn (dim "Bye!"))

        Just input
            | input == ":q" || input == ":quit" ->
                liftIO (putStrLn (dim "Bye!"))

            | null (strip input) ->
                loop rs rIn rOut

            | take 3 input == ":t " ->
                handleTypeQuery rs rIn rOut (drop 3 input)

            | otherwise ->
                handleInput rs rIn rOut input


handleTypeQuery :: ReplState -> Handle -> Handle -> String -> InputT IO ()
handleTypeQuery rs rIn rOut input = do
    fullInput <- readMultiLine input
    let
        tmpInput =
            "it <- " ++ fullInput
    case replCompile rs tmpInput of
        Right ( _, tyInfo, _ ) -> do
            let
                cleaned =
                    case stripPfx "it : " (strip tyInfo) of
                        Just rest ->
                            rest

                        Nothing ->
                            strip tyInfo
            liftIO (putStrLn (cyan cleaned))
            loop rs rIn rOut

        Left e -> do
            liftIO (putStrLn (red (stripDef e)))
            loop rs rIn rOut


handleInput :: ReplState -> Handle -> Handle -> String -> InputT IO ()
handleInput rs rIn rOut input = do
    fullInput <- readMultiLine input
    case replCompile rs fullInput of
        Right ( rCode, tyInfo, rs' ) -> do
            let
                rLines =
                    filter (not . null) (lines (strip rCode))

                cleaned =
                    strip tyInfo
            when (not (null rLines)) (liftIO (sendToR rIn rOut rLines))
            when (not (null cleaned))
                (liftIO (putStrLn (highlightTypeInfo cleaned)))
            loop rs' rIn rOut

        Left e -> do
            liftIO (putStrLn (red (stripDef e)))
            loop rs rIn rOut



-- --------------------------------------------------------------------
-- MULTI-LINE INPUT (Elm-style continuation)
-- --------------------------------------------------------------------


-- | Decide whether the input we have so far is "complete". If it isn't,
-- prompt for more lines.
readMultiLine :: String -> InputT IO String
readMultiLine acc
    | needsContinuation acc =
        readContinuation acc

    | otherwise =
        return acc


readContinuation :: String -> InputT IO String
readContinuation acc = do
    mcont <- getInputLine "| "
    case mcont of
        Nothing ->
            return acc

        Just line
            | null (strip line) ->
                return acc

            | otherwise ->
                let
                    newAcc =
                        acc ++ "\n" ++ line
                in
                if bracketsBalanced newAcc
                    && not (endsWithContinuation newAcc)
                    && not (isIndented line) then
                    return newAcc
                else
                    readContinuation newAcc


isIndented :: String -> Bool
isIndented s =
    case s of
        c : _ ->
            c == ' ' || c == '\t'

        _ ->
            False


needsContinuation :: String -> Bool
needsContinuation s =
    not (bracketsBalanced s) || endsWithContinuation s


-- | Walk the string, tracking depth in @()@, @[]@, and @{}@. Skips
-- past string literals and line comments.
bracketsBalanced :: String -> Bool
bracketsBalanced =
    go 0 0 0 False
  where
    go p b c _ "" =
        p == 0 && b == 0 && c == 0

    go p b c True ('\\' : _ : rest) = go p b c True rest
    go p b c True ('"' : rest) = go p b c False rest
    go p b c True (_ : rest) = go p b c True rest
    go p b c False ('"' : rest) = go p b c True rest
    go p b c False ('#' : rest) = go p b c False (dropWhile (/= '\n') rest)
    go p b c False ('(' : rest) = go (p + 1) b c False rest
    go p b c False (')' : rest) = go (max 0 (p - 1)) b c False rest
    go p b c False ('[' : rest) = go p (b + 1) c False rest
    go p b c False (']' : rest) = go p (max 0 (b - 1)) c False rest
    go p b c False ('{' : rest) = go p b (c + 1) False rest
    go p b c False ('}' : rest) = go p b (max 0 (c - 1)) False rest
    go p b c False (_ : rest) = go p b c False rest


-- | Heuristic: does this look like an unfinished expression?
endsWithContinuation :: String -> Bool
endsWithContinuation s =
    let
        trimmed =
            reverse
                (dropWhile
                    (\c -> c == ' ' || c == '\t' || c == '\n' || c == '\r')
                    (reverse s)
                )

        endsWith suffix str =
            drop (length str - length suffix) str == suffix
    in
    any (`endsWith` trimmed)
        [ "<-", "|>", "->", ",", "=", "\\", "dataframe", "record" ]



-- --------------------------------------------------------------------
-- TALKING TO R
-- --------------------------------------------------------------------


-- | Send a list of R lines to the R subprocess and print everything it
-- writes back, until we see our sentinel value.
sendToR :: Handle -> Handle -> [String] -> IO ()
sendToR rIn rOut rLines = do
    mapM_ (\l -> hPutStrLn rIn l >> hFlush rIn) rLines
    hPutStrLn rIn "cat(\"__QUONE_END__\\n\")"
    hFlush rIn
    collectOutput
  where
    collectOutput = do
        line <- hGetLine rOut
        if line == "__QUONE_END__" then
            return ()
        else do
            putStrLn line
            collectOutput



-- --------------------------------------------------------------------
-- LITTLE STRING HELPERS
-- --------------------------------------------------------------------


stripPfx :: String -> String -> Maybe String
stripPfx [] ys =
    Just ys

stripPfx _ [] =
    Nothing

stripPfx (x : xs) (y : ys)
    | x == y =
        stripPfx xs ys

    | otherwise =
        Nothing


stripDef :: String -> String
stripDef s =
    case stripPfx "In definition of 'it': " s of
        Just rest ->
            rest

        Nothing ->
            s


strip :: String -> String
strip =
    reverse . dropWhile isSpace' . reverse . dropWhile isSpace'
  where
    isSpace' c =
        c == ' ' || c == '\n' || c == '\t' || c == '\r'



-- --------------------------------------------------------------------
-- ANSI COLORS
-- --------------------------------------------------------------------


ansi :: String -> String -> String
ansi code s =
    "\ESC[" ++ code ++ "m" ++ s ++ "\ESC[0m"


bold :: String -> String
bold = ansi "1"


dim :: String -> String
dim = ansi "2"


red :: String -> String
red = ansi "31"


cyan :: String -> String
cyan = ansi "36"


-- | Highlight a "name : type" line. The name gets green, the colon dim,
-- the type cyan.
highlightTypeInfo :: String -> String
highlightTypeInfo s =
    case break (== ':') s of
        ( name, ':' : rest ) ->
            ansi "32" name ++ dim " :" ++ cyan rest

        _ ->
            cyan s
