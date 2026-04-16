{-|
Module: Main (the @quone-lsp@ executable)

A small Language Server Protocol implementation for Quone. The editor
talks to this process over stdin/stdout in JSON-RPC; we publish three
kinds of feedback:

  - Diagnostics on open/change/save
  - Hover info when the user points at an identifier
  - Document formatting on demand

The server keeps a 'TVar' from URI to text so we always have the latest
copy of every open document. Everything else is handled inside the
@lsp@ library's request/notification handlers.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Concurrent.STM
    ( TVar
    , atomically
    , modifyTVar'
    , newTVarIO
    , readTVarIO
    )
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
    ( Diagnostic(..)
    , DiagnosticSeverity(..)
    , DidChangeTextDocumentParams(..)
    , DidCloseTextDocumentParams(..)
    , DidOpenTextDocumentParams(..)
    , DidSaveTextDocumentParams(..)
    , DocumentFormattingParams(..)
    , Hover(..)
    , HoverParams(..)
    , MarkupContent(..)
    , MarkupKind(..)
    , Null(..)
    , Position(..)
    , PublishDiagnosticsParams(..)
    , Range(..)
    , SaveOptions(..)
    , TextDocumentContentChangeEvent(..)
    , TextDocumentContentChangePartial(..)
    , TextDocumentContentChangeWholeDocument(..)
    , TextDocumentIdentifier(..)
    , TextDocumentItem(..)
    , TextDocumentSyncKind(..)
    , TextDocumentSyncOptions(..)
    , TextEdit(..)
    , Uri
    , VersionedTextDocumentIdentifier(..)
    , type (|?)(..)
    )
import Language.LSP.Server

import qualified Quone



-- ====================================================================
-- SERVER STATE
-- ====================================================================


-- | A shared map from document URI to its current text. We update this
-- on every notification so handlers always see the latest version.
type DocMap = TVar (Map.Map Uri T.Text)


newtype ServerState = ServerState
    { documents :: DocMap
    }



-- ====================================================================
-- ENTRY POINT
-- ====================================================================


main :: IO Int
main = do
    docs <- newTVarIO Map.empty
    let
        state = ServerState docs
    runServer
        ServerDefinition
            { parseConfig = const (const (Right ()))
            , onConfigChange = const (pure ())
            , defaultConfig = ()
            , configSection = "quone"
            , doInitialize = \env _req -> pure (Right env)
            , staticHandlers = \_caps -> handlers state
            , interpretHandler = \env -> Iso (runLspT env) liftIO
            , options = serverOptions
            }


-- | Options describing what features the server supports. We need full
-- text sync (we resend the whole document) and we want to be notified
-- on save.
serverOptions :: Options
serverOptions =
    defaultOptions
        { optTextDocumentSync =
            Just $
                TextDocumentSyncOptions
                    (Just True)
                    (Just TextDocumentSyncKind_Full)
                    (Just False)
                    (Just False)
                    (Just (InR (SaveOptions (Just True))))
        }



-- ====================================================================
-- HANDLERS
-- ====================================================================


handlers :: ServerState -> Handlers (LspM ())
handlers state =
    mconcat
        [ initializedHandler
        , didOpenHandler state
        , didChangeHandler state
        , didSaveHandler state
        , didCloseHandler state
        , hoverHandler state
        , formattingHandler state
        ]


initializedHandler :: Handlers (LspM ())
initializedHandler =
    notificationHandler SMethod_Initialized $ \_msg -> pure ()


didOpenHandler :: ServerState -> Handlers (LspM ())
didOpenHandler state =
    notificationHandler SMethod_TextDocumentDidOpen $ \msg -> do
        let
            TNotificationMessage _ _ params =
                msg

            DidOpenTextDocumentParams (TextDocumentItem uri _ _ txt) =
                params
        liftIO (storeDocument state uri txt)
        publishDiags uri txt


didChangeHandler :: ServerState -> Handlers (LspM ())
didChangeHandler state =
    notificationHandler SMethod_TextDocumentDidChange $ \msg -> do
        let
            TNotificationMessage _ _ params =
                msg

            DidChangeTextDocumentParams (VersionedTextDocumentIdentifier uri _) changes =
                params
        case changes of
            [] ->
                pure ()

            _ -> do
                let
                    txt =
                        getChangeText (last changes)
                liftIO (storeDocument state uri txt)
                publishDiags uri txt


didSaveHandler :: ServerState -> Handlers (LspM ())
didSaveHandler state =
    notificationHandler SMethod_TextDocumentDidSave $ \msg -> do
        let
            TNotificationMessage _ _ params =
                msg

            DidSaveTextDocumentParams (TextDocumentIdentifier uri) mtext =
                params
        case mtext of
            Just txt -> do
                liftIO (storeDocument state uri txt)
                publishDiags uri txt

            Nothing ->
                pure ()


didCloseHandler :: ServerState -> Handlers (LspM ())
didCloseHandler state =
    notificationHandler SMethod_TextDocumentDidClose $ \msg -> do
        let
            TNotificationMessage _ _ params =
                msg

            DidCloseTextDocumentParams (TextDocumentIdentifier uri) =
                params
        liftIO $
            atomically $
                modifyTVar' (documents state) (Map.delete uri)
        sendNotification SMethod_TextDocumentPublishDiagnostics
            (PublishDiagnosticsParams uri Nothing [])


hoverHandler :: ServerState -> Handlers (LspM ())
hoverHandler state =
    requestHandler SMethod_TextDocumentHover $ \req responder -> do
        let
            TRequestMessage _ _ _ params =
                req

            HoverParams (TextDocumentIdentifier uri) (Position l c) _ =
                params

            line =
                fromIntegral l :: Int

            col =
                fromIntegral c :: Int
        docs <- liftIO (readTVarIO (documents state))
        case Map.lookup uri docs of
            Nothing ->
                responder (Right (InR Null))

            Just txt ->
                respondHover responder (T.unpack txt) line col


respondHover
    :: (Either e (Hover |? Null) -> LspM () ()) -> String -> Int -> Int -> LspM () ()
respondHover responder source line col =
    case findHover source line col of
        Nothing ->
            responder (Right (InR Null))

        Just content -> do
            let
                ms =
                    MarkupContent
                        MarkupKind_Markdown
                        (T.pack ("```quone\n" ++ content ++ "\n```"))

                rsp =
                    Hover (InL ms) Nothing
            responder (Right (InL rsp))


formattingHandler :: ServerState -> Handlers (LspM ())
formattingHandler state =
    requestHandler SMethod_TextDocumentFormatting $ \req responder -> do
        let
            TRequestMessage _ _ _ params =
                req

            DocumentFormattingParams _ (TextDocumentIdentifier uri) _ =
                params
        docs <- liftIO (readTVarIO (documents state))
        case Map.lookup uri docs of
            Nothing ->
                responder (Right (InR Null))

            Just txt ->
                respondFormat responder txt


respondFormat
    :: (Either e ([TextEdit] |? Null) -> LspM () ())
    -> T.Text
    -> LspM () ()
respondFormat responder txt = do
    let
        source =
            T.unpack txt
    case Quone.formatSource source of
        Left _ ->
            responder (Right (InR Null))

        Right formatted -> do
            let
                edit =
                    fullDocumentEdit txt (T.pack formatted)
            responder (Right (InL [ edit ]))



-- ====================================================================
-- HELPERS
-- ====================================================================


-- | Add or replace the document text in our shared TVar.
storeDocument :: ServerState -> Uri -> T.Text -> IO ()
storeDocument state uri txt =
    atomically (modifyTVar' (documents state) (Map.insert uri txt))


-- | Pull the new text out of one of LSP's two change-event shapes.
getChangeText :: TextDocumentContentChangeEvent -> T.Text
getChangeText e =
    case e of
        TextDocumentContentChangeEvent (InL (TextDocumentContentChangePartial _ _ txt)) ->
            txt

        TextDocumentContentChangeEvent (InR (TextDocumentContentChangeWholeDocument txt)) ->
            txt


-- | A 'TextEdit' that replaces the whole document with new text.
fullDocumentEdit :: T.Text -> T.Text -> TextEdit
fullDocumentEdit oldText newText =
    let
        srcLines =
            T.lines oldText

        lastLine =
            if null srcLines then 0 else length srcLines - 1

        lastCol =
            if null srcLines then 0 else T.length (last srcLines)

        range =
            Range
                (Position 0 0)
                (Position (fromIntegral lastLine) (fromIntegral lastCol))
    in
    TextEdit range newText



-- ====================================================================
-- DIAGNOSTICS
-- ====================================================================


-- | Type-check the source and publish any errors as diagnostics.
publishDiags :: Uri -> T.Text -> LspM () ()
publishDiags uri txt = do
    let
        source =
            T.unpack txt

        srcLines =
            lines source

        errors =
            Quone.check source

        diags =
            map (toDiag srcLines) errors
    liftIO $
        appendFile
            "/tmp/quone-lsp.log"
            ( "publishDiags: uri="
                ++ show uri
                ++ " errors="
                ++ show (length errors)
                ++ concatMap
                    (\e ->
                        "\n  "
                            ++ show (Quone.ceLine e)
                            ++ ":"
                            ++ show (Quone.ceCol e)
                            ++ " "
                            ++ Quone.ceMessage e
                    )
                    errors
                ++ "\n"
            )
    sendNotification SMethod_TextDocumentPublishDiagnostics
        (PublishDiagnosticsParams uri Nothing diags)


-- | Convert one 'Quone.CompileError' into an LSP 'Diagnostic'.
toDiag :: [String] -> Quone.CompileError -> Diagnostic
toDiag srcLines err =
    let
        line =
            max 0 (Quone.ceLine err - 1)

        col =
            max 0 (Quone.ceCol err - 1)

        endCol =
            case drop line srcLines of
                l : _ -> findExprEnd col l
                [] -> col + 1

        msg =
            stripPrefix' (Quone.ceMessage err)
    in
    Diagnostic
        ( Range
            (Position (fromIntegral line) (fromIntegral col))
            (Position (fromIntegral line) (fromIntegral endCol))
        )
        (Just DiagnosticSeverity_Error)
        Nothing
        Nothing
        (Just "quone")
        (T.pack msg)
        Nothing
        Nothing
        Nothing


-- | Drop the "In definition of '...': " wrapper that the inferencer
-- attaches to errors, since the editor already shows the location.
stripPrefix' :: String -> String
stripPrefix' s =
    case stripInDef s of
        Just rest -> rest
        Nothing -> s
  where
    stripInDef msg
        | take 19 msg == "In definition of '" =
            let
                afterName =
                    drop 1 (dropWhile (/= '\'') (drop 19 msg))
            in
            case afterName of
                ':' : ' ' : rest -> Just rest
                ':' : rest -> Just rest
                _ -> Nothing

        | otherwise =
            Nothing


-- | Find a sensible "end column" for highlighting a problem expression,
-- so the editor underlines just the bad bit and not the whole line.
findExprEnd :: Int -> String -> Int
findExprEnd startCol srcLine =
    let
        rest =
            drop startCol srcLine

        rawLen =
            scan rest 0 (0 :: Int)

        trimLen =
            rawLen - countTrailing (take rawLen rest)
    in
    startCol + max 1 trimLen
  where
    scan [] _ acc = acc
    scan (',' : _) 0 acc = acc
    scan ('}' : _) 0 acc = acc
    scan ('(' : cs) d acc = scan cs (d + 1) (acc + 1)
    scan (')' : cs) d acc = scan cs (max 0 (d - 1)) (acc + 1)
    scan (_ : cs) d acc = scan cs d (acc + 1)

    countTrailing s =
        length (takeWhile (== ' ') (reverse s))



-- ====================================================================
-- HOVER LOOKUP
-- ====================================================================


-- | Find the identifier at the given line/column and return a string
-- of the form @"name : type"@ if we have one for it.
findHover :: String -> Int -> Int -> Maybe String
findHover source line col = do
    let
        srcLines =
            lines source
    targetLine <-
        case drop line srcLines of
            l : _ -> Just l
            [] -> Nothing
    let
        word =
            extractWordAt targetLine col
    if null word then
        Nothing
    else
        let
            entries =
                Quone.hoverInfo source
        in
        case filter (\e -> Quone.heName e == word) entries of
            e : _ -> Just (Quone.heName e ++ " : " ++ Quone.heTypeStr e)
            [] -> Nothing


-- | Pull the identifier at @col@ out of @line@. Returns "" if the cursor
-- isn't on an identifier character.
extractWordAt :: String -> Int -> String
extractWordAt line col
    | col >= length line =
        ""

    | not (isIdent (line !! col)) =
        ""

    | otherwise =
        let
            start =
                goBack col

            end =
                goFwd col
        in
        take (end - start) (drop start line)
  where
    isIdent c =
        c `elem` (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "_")

    goBack i
        | i > 0 && isIdent (line !! (i - 1)) =
            goBack (i - 1)

        | otherwise =
            i

    goFwd i
        | i < length line && isIdent (line !! i) =
            goFwd (i + 1)

        | otherwise =
            i
