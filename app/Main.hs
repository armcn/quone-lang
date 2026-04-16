module Main where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.FilePath (takeBaseName, (</>))
import System.IO (hFlush, hSetBuffering, BufferMode(..), hPutStrLn, hGetLine, hReady)
import System.Process (createProcess, proc, std_in, std_out, std_err, StdStream(..), ProcessHandle)
import GHC.IO.Handle (Handle)
import System.Console.Haskeline
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try, SomeException)

import Quone

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  case args of
    [] -> do
      putStrLn $ "Usage: " ++ progName ++ " <input.Q> [--package | --format]"
      putStrLn $ "       " ++ progName ++ " repl"
      exitFailure
    ["repl"] -> runRepl
    (input:flags) -> do
      let packageMode = "--package" `elem` flags
          formatMode  = "--format" `elem` flags
      source <- readFile input

      if formatMode
        then case formatSource source of
          Right formatted -> putStr formatted
          Left e -> do
            putStrLn $ "Format error: " ++ e
            exitFailure
        else do
          let errors = check source
          case errors of
            (e:_) -> do
              putStrLn $ input ++ ":" ++ show (ceLine e) ++ ":" ++ show (ceCol e) ++ ": " ++ ceMessage e
              exitFailure
            [] -> return ()
          result <- case compile source of
            Right r  -> return r
            Left e -> do
              putStrLn $ input ++ ": " ++ e
              exitFailure

          if packageMode
            then case crModule result of
              Nothing -> do
                putStrLn "Error: --package requires a package declaration"
                putStrLn "  Add: package MyPackage exporting (..)"
                exitFailure
              Just info -> do
                let pkgDir = miName info
                    rDir   = pkgDir </> "R"
                    stem   = takeBaseName input
                createDirectoryIfMissing True rDir
                writeFile (rDir </> stem ++ ".R") (crRCode result)
                writeFile (pkgDir </> "NAMESPACE") (generateNamespace info)
                writeFile (pkgDir </> "DESCRIPTION") (generateDescription info)
                putStrLn $ "Package created: " ++ pkgDir ++ "/"
                putStrLn $ "  " ++ pkgDir ++ "/DESCRIPTION"
                putStrLn $ "  " ++ pkgDir ++ "/NAMESPACE"
                putStrLn $ "  " ++ pkgDir ++ "/R/" ++ stem ++ ".R"
            else putStr (crRCode result)

runRepl :: IO ()
runRepl = do
  putStrLn $ dim "Quone REPL — type :q to quit, :t <expr> to show type"
  putStrLn $ dim "Shortcuts: Alt+- inserts <-  Alt+m inserts |>"
  prefs <- loadPrefs
  rProc <- startR
  case rProc of
    Nothing -> do
      putStrLn $ red "Error: could not start R. Make sure R is installed."
      exitFailure
    Just (rIn, rOut, _ph) -> do
      hSetBuffering rIn LineBuffering
      hSetBuffering rOut LineBuffering
      drainInitial rOut
      let settings = defaultSettings { historyFile = Just ".quone_history" }
      runInputTWithPrefs prefs settings (loop newReplState rIn rOut)

loadPrefs :: IO Prefs
loadPrefs = do
  tmpDir <- getTemporaryDirectory
  let prefsFile = tmpDir </> "quone-haskeline"
  writeFile prefsFile $ unlines
    [ "bind: meta-- < -"
    , "bind: meta-m | >"
    ]
  readPrefs prefsFile

drainInitial :: Handle -> IO ()
drainInitial h = do
  ready <- hReady h
  when ready $ do
    _ <- hGetLine h
    drainInitial h

startR :: IO (Maybe (Handle, Handle, ProcessHandle))
startR = do
  let cp = (proc "R" ["--no-save", "--quiet", "--no-echo"])
             { std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit }
  result <- (try (createProcess cp)) :: IO (Either SomeException (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle))
  case result of
    Right (Just hin, Just hout, _, ph) -> return (Just (hin, hout, ph))
    _ -> return Nothing

loop :: ReplState -> Handle -> Handle -> InputT IO ()
loop rs rIn rOut = do
  minput <- getInputLine (bold "quone" ++ dim "> ")
  case minput of
    Nothing -> liftIO $ putStrLn (dim "Bye!")
    Just input
      | input == ":q" || input == ":quit" -> liftIO $ putStrLn (dim "Bye!")
      | null (strip input) -> loop rs rIn rOut
      | take 3 input == ":t " -> do
          fullInput <- readMultiLine (drop 3 input)
          let tmpInput = "it <- " ++ fullInput
          case replCompile rs tmpInput of
            Right (_, tyInfo, _) -> do
              let cleaned = case stripPfx "it : " (strip tyInfo) of
                              Just rest -> rest
                              Nothing   -> strip tyInfo
              liftIO $ putStrLn $ cyan cleaned
              loop rs rIn rOut
            Left e -> do
              liftIO $ putStrLn $ red (stripDef e)
              loop rs rIn rOut
      | otherwise -> do
          fullInput <- readMultiLine input
          case replCompile rs fullInput of
            Right (rCode, tyInfo, rs') -> do
              let rLines = filter (not . null) (lines (strip rCode))
                  cleaned = strip tyInfo
              when (not (null rLines)) $
                liftIO $ sendToR rIn rOut rLines
              when (not (null cleaned)) $
                liftIO $ putStrLn $ highlightTypeInfo cleaned
              loop rs' rIn rOut
            Left e -> do
              liftIO $ putStrLn $ red (stripDef e)
              loop rs rIn rOut

readMultiLine :: String -> InputT IO String
readMultiLine acc
  | needsContinuation acc = readContinuation acc
  | otherwise = return acc

readContinuation :: String -> InputT IO String
readContinuation acc = do
  mcont <- getInputLine "| "
  case mcont of
    Nothing -> return acc
    Just line
      | null (strip line) -> return acc
      | otherwise ->
          let newAcc = acc ++ "\n" ++ line
          in if bracketsBalanced newAcc
                && not (endsWithContinuation newAcc)
                && not (isIndented line)
             then return newAcc
             else readContinuation newAcc

isIndented :: String -> Bool
isIndented (c:_) = c == ' ' || c == '\t'
isIndented _ = False

needsContinuation :: String -> Bool
needsContinuation s = not (bracketsBalanced s) || endsWithContinuation s

bracketsBalanced :: String -> Bool
bracketsBalanced = go 0 0 0 False
  where
    go p b c _ [] = p == 0 && b == 0 && c == 0
    go p b c True  ('\\':_:rest) = go p b c True rest
    go p b c True  ('"':rest)    = go p b c False rest
    go p b c True  (_:rest)      = go p b c True rest
    go p b c False ('"':rest)    = go p b c True rest
    go p b c False ('#':rest)    = go p b c False (dropWhile (/= '\n') rest)
    go p b c False ('(':rest)    = go (p+1) b c False rest
    go p b c False (')':rest)    = go (max 0 (p-1)) b c False rest
    go p b c False ('[':rest)    = go p (b+1) c False rest
    go p b c False (']':rest)    = go p (max 0 (b-1)) c False rest
    go p b c False ('{':rest)    = go p b (c+1) False rest
    go p b c False ('}':rest)    = go p b (max 0 (c-1)) False rest
    go p b c False (_:rest)      = go p b c False rest

endsWithContinuation :: String -> Bool
endsWithContinuation s =
  let trimmed = reverse (dropWhile (\c -> c == ' ' || c == '\t' || c == '\n' || c == '\r') (reverse s))
  in any (`endsWith` trimmed) ["<-", "|>", "->", ",", "=", "\\", "dataframe", "record"]
  where
    endsWith suffix str = drop (length str - length suffix) str == suffix

sendToR :: Handle -> Handle -> [String] -> IO ()
sendToR rIn rOut rLines = do
  mapM_ (\l -> hPutStrLn rIn l >> hFlush rIn) rLines
  hPutStrLn rIn "cat(\"__QUONE_END__\\n\")"
  hFlush rIn
  collectOutput rOut
  where
    collectOutput h = do
      line <- hGetLine h
      if line == "__QUONE_END__"
        then return ()
        else do
          putStrLn line
          collectOutput h

stripPfx :: String -> String -> Maybe String
stripPfx [] ys = Just ys
stripPfx _ [] = Nothing
stripPfx (x:xs) (y:ys)
  | x == y    = stripPfx xs ys
  | otherwise = Nothing

stripDef :: String -> String
stripDef s = case stripPfx "In definition of 'it': " s of
  Just rest -> rest
  Nothing   -> s

strip :: String -> String
strip = reverse . dropWhile isSpace' . reverse . dropWhile isSpace'
  where isSpace' c = c == ' ' || c == '\n' || c == '\t' || c == '\r'

-- ANSI colors
ansi :: String -> String -> String
ansi code s = "\ESC[" ++ code ++ "m" ++ s ++ "\ESC[0m"

bold :: String -> String
bold = ansi "1"

dim :: String -> String
dim = ansi "2"

red :: String -> String
red = ansi "31"

green :: String -> String
green = ansi "32"

yellow :: String -> String
yellow = ansi "33"

cyan :: String -> String
cyan = ansi "36"

highlightTypeInfo :: String -> String
highlightTypeInfo s = case break (== ':') s of
  (name, ':':rest) -> green name ++ dim " :" ++ cyan rest
  _ -> cyan s
