{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards   #-}

-- | MCP (Model Context Protocol) Server
--   JSON-RPC 2.0 over stdio for AI agent integration.
module Spinor.MCP
  ( runMcpServer
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Aeson
import Data.Aeson.Types (Pair)
import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.Generics (Generic)
import Control.Monad (forever)
import System.IO (hFlush, stdout, hSetBuffering, stdin, BufferMode(..))
import Data.IORef

import Spinor.Syntax (Expr, readExpr, SpinorError(..), formatError)
import Spinor.Val (Val(..), Context, showVal, initialContext, Package(..), ctxPackages)
import Spinor.Eval (Eval, EvalState(..), runEvalWithContext)
import Spinor.Expander (expandAndEval, expand)
import Spinor.Infer (runInfer, infer, baseTypeEnv)
import Spinor.Type (TypeEnv, showType)
import Spinor.Primitive (primitiveBindings)
import Spinor.Server (exprToLispText)

-- ============================================================
-- JSON-RPC 2.0 Types
-- ============================================================

-- | JSON-RPC Request
data JsonRpcRequest = JsonRpcRequest
  { reqJsonrpc :: Text
  , reqId      :: Maybe Value
  , reqMethod  :: Text
  , reqParams  :: Maybe Value
  } deriving (Show, Generic)

instance FromJSON JsonRpcRequest where
  parseJSON = withObject "JsonRpcRequest" $ \v -> JsonRpcRequest
    <$> v .: "jsonrpc"
    <*> v .:? "id"
    <*> v .: "method"
    <*> v .:? "params"

-- | JSON-RPC Response
data JsonRpcResponse = JsonRpcResponse
  { resJsonrpc :: Text
  , resId      :: Value
  , resResult  :: Maybe Value
  , resError   :: Maybe JsonRpcError
  } deriving (Show, Generic)

instance ToJSON JsonRpcResponse where
  toJSON JsonRpcResponse{..} = object $ filter notNull
    [ "jsonrpc" .= resJsonrpc
    , "id"      .= resId
    , "result"  .= resResult
    , "error"   .= resError
    ]
    where
      notNull :: Pair -> Bool
      notNull (_, Null) = False
      notNull _         = True

-- | JSON-RPC Error
data JsonRpcError = JsonRpcError
  { errCode    :: Int
  , errMessage :: Text
  , errData    :: Maybe Value
  } deriving (Show, Generic)

instance ToJSON JsonRpcError where
  toJSON JsonRpcError{..} = object
    [ "code"    .= errCode
    , "message" .= errMessage
    , "data"    .= errData
    ]

-- ============================================================
-- MCP Protocol Types
-- ============================================================

-- | MCP Tool Definition
data McpTool = McpTool
  { toolName        :: Text
  , toolDescription :: Text
  , toolInputSchema :: Value
  } deriving (Show)

instance ToJSON McpTool where
  toJSON McpTool{..} = object
    [ "name"        .= toolName
    , "description" .= toolDescription
    , "inputSchema" .= toolInputSchema
    ]

-- | MCP Server State
data McpState = McpState
  { mcpContext  :: Context       -- ^ Package context
  , mcpTypeEnv  :: TypeEnv       -- ^ Type environment
  }

-- ============================================================
-- MCP Server Entry Point
-- ============================================================

-- | Run the MCP server (stdio event loop)
runMcpServer :: IO ()
runMcpServer = do
  -- Disable buffering for real-time communication
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering

  -- Initialize state with Twister library loaded
  stateRef <- newIORef =<< initMcpState

  -- Event loop
  forever $ do
    line <- TIO.getLine
    case eitherDecode (BL.fromStrict $ TE.encodeUtf8 line) of
      Left err -> do
        let resp = errorResponse Null (-32700) ("Parse error: " <> T.pack err) Nothing
        sendResponse resp
      Right req -> do
        resp <- handleRequest stateRef req
        sendResponse resp

-- | Initialize MCP state (load Twister library)
initMcpState :: IO McpState
initMcpState = do
  -- Load Twister library
  let prims = primitiveBindings
  result <- runEvalWithContext prims initialContext (loadTwister)
  case result of
    Left err -> do
      TIO.hPutStrLn stdout $ "Warning: Failed to load Twister: " <> formatError err
      pure $ McpState initialContext baseTypeEnv
    Right (_, st) -> do
      pure $ McpState (esContext st) baseTypeEnv
  where
    loadTwister :: Eval Val
    loadTwister = do
      -- Try to load twister/prelude.spin
      _ <- expandAndEval =<< readExprOrFail "(load \"twister/prelude.spin\")"
      pure (VBool True)

    readExprOrFail :: Text -> Eval Expr
    readExprOrFail code = case readExpr code of
      Left err   -> error $ "Parse error: " <> err
      Right expr -> pure expr

-- | Send JSON-RPC response
sendResponse :: JsonRpcResponse -> IO ()
sendResponse resp = do
  BL.putStrLn (encode resp)
  hFlush stdout

-- ============================================================
-- Request Handlers
-- ============================================================

-- | Handle incoming JSON-RPC request
handleRequest :: IORef McpState -> JsonRpcRequest -> IO JsonRpcResponse
handleRequest stateRef req = case reqMethod req of
  "initialize"   -> handleInitialize req
  "tools/list"   -> handleToolsList req
  "tools/call"   -> handleToolsCall stateRef req
  _method        -> pure $ errorResponse (maybe Null id $ reqId req)
                                         (-32601)
                                         ("Method not found: " <> _method)
                                         Nothing

-- | Handle initialize request
handleInitialize :: JsonRpcRequest -> IO JsonRpcResponse
handleInitialize req = pure $ successResponse (maybe Null id $ reqId req) $ object
  [ "protocolVersion" .= ("2024-11-05" :: Text)
  , "serverInfo" .= object
      [ "name"    .= ("spinor-mcp" :: Text)
      , "version" .= ("0.1.0" :: Text)
      ]
  , "capabilities" .= object
      [ "tools" .= object []
      ]
  ]

-- | Handle tools/list request
handleToolsList :: JsonRpcRequest -> IO JsonRpcResponse
handleToolsList req = pure $ successResponse (maybe Null id $ reqId req) $ object
  [ "tools" .= mcpTools
  ]

-- | Handle tools/call request
handleToolsCall :: IORef McpState -> JsonRpcRequest -> IO JsonRpcResponse
handleToolsCall stateRef req = do
  case reqParams req of
    Nothing -> pure $ errorResponse (reqIdOrNull req) (-32602) "Missing params" Nothing
    Just params -> case parseToolCall params of
      Left err -> pure $ errorResponse (reqIdOrNull req) (-32602) err Nothing
      Right (toolName', args) -> do
        result <- callTool stateRef toolName' args
        pure $ case result of
          Left err -> errorResponse (reqIdOrNull req) (-32000) err Nothing
          Right val -> successResponse (reqIdOrNull req) $ object
            [ "content" .= [ object
                [ "type" .= ("text" :: Text)
                , "text" .= val
                ]
              ]
            ]
  where
    reqIdOrNull = maybe Null id . reqId

-- | Parse tool call parameters
parseToolCall :: Value -> Either Text (Text, Maybe Value)
parseToolCall val = case val of
  Object obj -> do
    name <- case Map.lookup "name" (toKvList obj) of
      Just (String n) -> Right n
      _               -> Left "Missing or invalid 'name' field"
    let args = Map.lookup "arguments" (toKvList obj)
    Right (name, args)
  _ -> Left "Invalid params format"
  where
    toKvList obj = Map.fromList $ map (\(k, v) -> (keyToText k, v)) $ toList (Object obj)

    toList (Object o) = case fromJSON (Object o) of
      Success m -> Map.toList (m :: Map.Map Text Value)
      Error _   -> []
    toList _ = []

    keyToText k = case fromJSON (String k) of
      Success t -> t
      Error _   -> k

-- | Call a tool by name
callTool :: IORef McpState -> Text -> Maybe Value -> IO (Either Text Text)
callTool stateRef toolName' args = case toolName' of
  "eval"        -> callEval stateRef args
  "typecheck"   -> callTypecheck stateRef args
  "macroexpand" -> callMacroexpand stateRef args
  "list-symbols" -> callListSymbols stateRef args
  _             -> pure $ Left $ "Unknown tool: " <> toolName'

-- ============================================================
-- Tool Implementations
-- ============================================================

-- | eval: Evaluate Spinor code
callEval :: IORef McpState -> Maybe Value -> IO (Either Text Text)
callEval stateRef args = do
  case extractCode args of
    Left err -> pure $ Left err
    Right code -> do
      state <- readIORef stateRef
      case readExpr code of
        Left err -> pure $ Left $ "Parse error: " <> T.pack err
        Right expr -> do
          result <- runEvalWithContext primitiveBindings (mcpContext state) (expandAndEval expr)
          case result of
            Left err -> pure $ Left $ formatError err
            Right (val, newSt) -> do
              writeIORef stateRef state { mcpContext = esContext newSt }
              pure $ Right $ T.pack $ showVal val

-- | typecheck: Type-check Spinor code
callTypecheck :: IORef McpState -> Maybe Value -> IO (Either Text Text)
callTypecheck stateRef args = do
  case extractCode args of
    Left err -> pure $ Left err
    Right code -> do
      state <- readIORef stateRef
      case readExpr code of
        Left err -> pure $ Left $ "Parse error: " <> T.pack err
        Right expr -> do
          -- Run type inference
          let typeEnv = mcpTypeEnv state
          case runInfer (infer typeEnv expr) of
            Left (SpinorError _ msg) -> pure $ Left msg
            Right (_, typ) -> pure $ Right $ showType typ

-- | macroexpand: Expand macros in Spinor code
callMacroexpand :: IORef McpState -> Maybe Value -> IO (Either Text Text)
callMacroexpand stateRef args = do
  case extractCode args of
    Left err -> pure $ Left err
    Right code -> do
      state <- readIORef stateRef
      case readExpr code of
        Left err -> pure $ Left $ "Parse error: " <> T.pack err
        Right expr -> do
          result <- runEvalWithContext primitiveBindings (mcpContext state) (expand expr)
          case result of
            Left err -> pure $ Left $ formatError err
            Right (expanded, _) -> pure $ Right $ exprToLispText expanded

-- | list-symbols: List available symbols
callListSymbols :: IORef McpState -> Maybe Value -> IO (Either Text Text)
callListSymbols stateRef args = do
  state <- readIORef stateRef
  let pkgName = extractPackage args
      ctx = mcpContext state
  case pkgName of
    Nothing -> do
      -- List all packages and their exported symbols
      let packages = Map.toList (ctxPackages ctx)
          formatPkg (name, pkg) = name <> ": " <> T.intercalate ", " (Set.toList $ pkgExports pkg)
      pure $ Right $ T.intercalate "\n" $ map formatPkg packages
    Just name -> do
      case Map.lookup name (ctxPackages ctx) of
        Nothing -> pure $ Left $ "Package not found: " <> name
        Just pkg -> do
          let bindings = Map.keys (pkgBindings pkg)
              exports = Set.toList (pkgExports pkg)
          pure $ Right $ T.unlines
            [ "Package: " <> name
            , "Bindings: " <> T.intercalate ", " bindings
            , "Exports: " <> T.intercalate ", " exports
            ]

-- | Extract 'code' field from arguments
extractCode :: Maybe Value -> Either Text Text
extractCode Nothing = Left "Missing arguments"
extractCode (Just val) = case val of
  Object obj -> case Map.lookup "code" (objToMap obj) of
    Just (String code) -> Right code
    Just _             -> Left "'code' must be a string"
    Nothing            -> Left "Missing 'code' field"
  _ -> Left "Arguments must be an object"
  where
    objToMap obj = case fromJSON (Object obj) :: Result (Map.Map Text Value) of
      Success m -> m
      Error _   -> Map.empty

-- | Extract optional 'package' field from arguments
extractPackage :: Maybe Value -> Maybe Text
extractPackage Nothing = Nothing
extractPackage (Just val) = case val of
  Object obj -> case Map.lookup "package" (objToMap obj) of
    Just (String pkg) -> Just pkg
    _                 -> Nothing
  _ -> Nothing
  where
    objToMap obj = case fromJSON (Object obj) :: Result (Map.Map Text Value) of
      Success m -> m
      Error _   -> Map.empty

-- ============================================================
-- Tool Definitions
-- ============================================================

-- | Available MCP tools
mcpTools :: [McpTool]
mcpTools =
  [ McpTool
      { toolName = "eval"
      , toolDescription = "Evaluate Spinor code in the current context. Definitions persist across calls."
      , toolInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "code" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("Spinor code to evaluate" :: Text)
                  ]
              ]
          , "required" .= (["code"] :: [Text])
          ]
      }
  , McpTool
      { toolName = "typecheck"
      , toolDescription = "Infer the type of a Spinor expression using Hindley-Milner type inference."
      , toolInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "code" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("Spinor expression to type-check" :: Text)
                  ]
              ]
          , "required" .= (["code"] :: [Text])
          ]
      }
  , McpTool
      { toolName = "macroexpand"
      , toolDescription = "Expand macros in Spinor code and return the resulting S-expression."
      , toolInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "code" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("Spinor code with macros to expand" :: Text)
                  ]
              ]
          , "required" .= (["code"] :: [Text])
          ]
      }
  , McpTool
      { toolName = "list-symbols"
      , toolDescription = "List available symbols in the current context or a specific package."
      , toolInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "package" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("Optional package name to list symbols from" :: Text)
                  ]
              ]
          ]
      }
  ]

-- ============================================================
-- Helpers
-- ============================================================

-- | Create a success response
successResponse :: Value -> Value -> JsonRpcResponse
successResponse reqId' result = JsonRpcResponse
  { resJsonrpc = "2.0"
  , resId      = reqId'
  , resResult  = Just result
  , resError   = Nothing
  }

-- | Create an error response
errorResponse :: Value -> Int -> Text -> Maybe Value -> JsonRpcResponse
errorResponse reqId' code msg dat = JsonRpcResponse
  { resJsonrpc = "2.0"
  , resId      = reqId'
  , resResult  = Nothing
  , resError   = Just $ JsonRpcError code msg dat
  }

