module Main where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeBaseName, (</>))

import Quone

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  case args of
    [] -> do
      putStrLn $ "Usage: " ++ progName ++ " <input.Q> [--package | --format]"
      exitFailure
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
          result <- case compile source of
            Right r  -> return r
            Left e -> do
              putStrLn $ "Compile error: " ++ e
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
