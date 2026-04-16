{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Control.Concurrent.STM
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
  ( Uri, Null(..)
  , TextDocumentSyncOptions(..), TextDocumentSyncKind(..)
  , SaveOptions(..)
  , Position(..), Range(..)
  , Hover(..), MarkupContent(..), MarkupKind(..)
  , TextEdit(..)
  , Diagnostic(..), DiagnosticSeverity(..)
  , PublishDiagnosticsParams(..)
  , DidOpenTextDocumentParams(..)
  , DidChangeTextDocumentParams(..)
  , DidSaveTextDocumentParams(..)
  , DidCloseTextDocumentParams(..)
  , HoverParams(..)
  , DocumentFormattingParams(..)
  , TextDocumentItem(..)
  , VersionedTextDocumentIdentifier(..)
  , TextDocumentIdentifier(..)
  , TextDocumentContentChangeEvent(..)
  , TextDocumentContentChangePartial(..)
  , TextDocumentContentChangeWholeDocument(..)
  , type (|?) (..)
  )
import Language.LSP.Server

import qualified Quone

type DocMap = TVar (Map.Map Uri T.Text)

newtype ServerState = ServerState { documents :: DocMap }

main :: IO Int
main = do
  docs <- newTVarIO Map.empty
  let state = ServerState docs
  runServer $ ServerDefinition
    { parseConfig = const $ const $ Right ()
    , onConfigChange = const $ pure ()
    , defaultConfig = ()
    , configSection = "quone"
    , doInitialize = \env _req -> pure $ Right env
    , staticHandlers = \_caps -> handlers state
    , interpretHandler = \env -> Iso (runLspT env) liftIO
    , options = defaultOptions
        { optTextDocumentSync = Just $ TextDocumentSyncOptions
            (Just True)
            (Just TextDocumentSyncKind_Full)
            (Just False)
            (Just False)
            (Just $ InR $ SaveOptions (Just True))
        }
    }

handlers :: ServerState -> Handlers (LspM ())
handlers state = mconcat
  [ notificationHandler SMethod_Initialized $ \_msg -> pure ()

  , notificationHandler SMethod_TextDocumentDidOpen $ \msg -> do
      let TNotificationMessage _ _ (DidOpenTextDocumentParams (TextDocumentItem uri' _ _ txt)) = msg
      liftIO $ atomically $ modifyTVar' (documents state) (Map.insert uri' txt)
      publishDiags uri' txt

  , notificationHandler SMethod_TextDocumentDidChange $ \msg -> do
      let TNotificationMessage _ _ (DidChangeTextDocumentParams (VersionedTextDocumentIdentifier uri' _) changes) = msg
      case changes of
        [] -> pure ()
        _  -> do
          let txt = getChangeText (last changes)
          liftIO $ atomically $ modifyTVar' (documents state) (Map.insert uri' txt)
          publishDiags uri' txt

  , notificationHandler SMethod_TextDocumentDidSave $ \msg -> do
      let TNotificationMessage _ _ (DidSaveTextDocumentParams (TextDocumentIdentifier uri') mtext) = msg
      case mtext of
        Just txt -> do
          liftIO $ atomically $ modifyTVar' (documents state) (Map.insert uri' txt)
          publishDiags uri' txt
        Nothing -> pure ()

  , notificationHandler SMethod_TextDocumentDidClose $ \msg -> do
      let TNotificationMessage _ _ (DidCloseTextDocumentParams (TextDocumentIdentifier uri')) = msg
      liftIO $ atomically $ modifyTVar' (documents state) (Map.delete uri')
      sendNotification SMethod_TextDocumentPublishDiagnostics $
        PublishDiagnosticsParams uri' Nothing []

  , requestHandler SMethod_TextDocumentHover $ \req responder -> do
      let TRequestMessage _ _ _ (HoverParams (TextDocumentIdentifier uri') (Position l c) _) = req
          line = fromIntegral l :: Int
          col  = fromIntegral c :: Int
      docs <- liftIO $ readTVarIO (documents state)
      case Map.lookup uri' docs of
        Nothing -> responder (Right $ InR Null)
        Just txt -> do
          let source = T.unpack txt
              result = findHover source line col
          case result of
            Nothing -> responder (Right $ InR Null)
            Just content -> do
              let ms = MarkupContent MarkupKind_Markdown (T.pack $ "```quone\n" ++ content ++ "\n```")
                  rsp = Hover (InL ms) Nothing
              responder (Right $ InL rsp)

  , requestHandler SMethod_TextDocumentFormatting $ \req responder -> do
      let TRequestMessage _ _ _ (DocumentFormattingParams _ (TextDocumentIdentifier uri') _) = req
      docs <- liftIO $ readTVarIO (documents state)
      case Map.lookup uri' docs of
        Nothing -> responder (Right $ InR Null)
        Just txt -> do
          let source = T.unpack txt
          case Quone.formatSource source of
            Left _ -> responder (Right $ InR Null)
            Right formatted -> do
              let srcLines = T.lines txt
                  lastLine = if null srcLines then 0 else length srcLines - 1
                  lastCol  = if null srcLines then 0 else T.length (last srcLines)
                  range = Range
                    (Position 0 0)
                    (Position (fromIntegral lastLine) (fromIntegral lastCol))
                  edit = TextEdit range (T.pack formatted)
              responder (Right $ InL [edit])
  ]

getChangeText :: TextDocumentContentChangeEvent -> T.Text
getChangeText (TextDocumentContentChangeEvent (InL (TextDocumentContentChangePartial _ _ txt))) = txt
getChangeText (TextDocumentContentChangeEvent (InR (TextDocumentContentChangeWholeDocument txt))) = txt

publishDiags :: Uri -> T.Text -> LspM () ()
publishDiags uri' txt = do
  let source = T.unpack txt
      srcLines = lines source
      errors = Quone.check source
      diags = map (toDiag srcLines) errors
  liftIO $ appendFile "/tmp/quone-lsp.log" $
    "publishDiags: uri=" ++ show uri' ++ " errors=" ++ show (length errors) ++
    concatMap (\e -> "\n  " ++ show (Quone.ceLine e) ++ ":" ++ show (Quone.ceCol e) ++ " " ++ Quone.ceMessage e) errors ++ "\n"
  sendNotification SMethod_TextDocumentPublishDiagnostics $
    PublishDiagnosticsParams uri' Nothing diags

toDiag :: [String] -> Quone.CompileError -> Diagnostic
toDiag srcLines err =
  let line = max 0 (Quone.ceLine err - 1)
      col  = max 0 (Quone.ceCol err - 1)
      endCol = case drop line srcLines of
        (l:_) -> findExprEnd col l
        []    -> col + 1
      msg = stripPrefix' (Quone.ceMessage err)
  in Diagnostic
    (Range
      (Position (fromIntegral line) (fromIntegral col))
      (Position (fromIntegral line) (fromIntegral endCol)))
    (Just DiagnosticSeverity_Error)
    Nothing
    Nothing
    (Just "quone")
    (T.pack msg)
    Nothing
    Nothing
    Nothing

stripPrefix' :: String -> String
stripPrefix' s = case stripInDef s of
  Just rest -> rest
  Nothing   -> s
  where
    stripInDef msg
      | take 19 msg == "In definition of '" =
        let afterName = drop 1 (dropWhile (/= '\'') (drop 19 msg))
        in case afterName of
          ':':' ':rest -> Just rest
          ':':rest     -> Just rest
          _            -> Nothing
      | otherwise = Nothing

findExprEnd :: Int -> String -> Int
findExprEnd startCol srcLine =
  let rest = drop startCol srcLine
      rawLen = go rest 0 0
      trimLen = rawLen - countTrailing (take rawLen rest)
  in startCol + max 1 trimLen
  where
    go [] _ acc = acc
    go (',' : _) 0 acc = acc
    go ('}' : _) 0 acc = acc
    go ('(' : cs) d acc = go cs (d + 1) (acc + 1)
    go (')' : cs) d acc = go cs (max 0 (d - 1)) (acc + 1)
    go (_ : cs) d acc = go cs d (acc + 1)
    countTrailing s = length (takeWhile (== ' ') (reverse s))

findHover :: String -> Int -> Int -> Maybe String
findHover source line col = do
  let srcLines = lines source
  targetLine <- case drop line srcLines of
    (l:_) -> Just l
    []    -> Nothing
  let word = extractWordAt targetLine col
  if null word
    then Nothing
    else let entries = Quone.hoverInfo source
         in case filter (\e -> Quone.heName e == word) entries of
              (e:_) -> Just (Quone.heName e ++ " : " ++ Quone.heTypeStr e)
              []    -> Nothing

extractWordAt :: String -> Int -> String
extractWordAt line col
  | col >= length line = ""
  | not (isIdent (line !! col)) = ""
  | otherwise =
    let start = goBack col
        end'  = goFwd col
    in take (end' - start) (drop start line)
  where
    isIdent c = c `elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")
    goBack i
      | i > 0 && isIdent (line !! (i - 1)) = goBack (i - 1)
      | otherwise = i
    goFwd i
      | i < length line && isIdent (line !! i) = goFwd (i + 1)
      | otherwise = i
