{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | LLVM Backend for Spinor
--
-- Provides LLVM IR generation and JIT execution for integer arithmetic.
-- Enabled via @cabal build -f llvm@.
--
-- Supported expressions:
--   - Integer literals
--   - @(+ e1 e2)@, @(- e1 e2)@, @(* e1 e2)@
module Spinor.Compiler.LLVM
  ( runJIT
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Spinor.Syntax (Expr(..))

#ifdef USE_LLVM

import Data.Int (Int64)
import Control.Exception (try, SomeException)

import qualified LLVM.Core as LC
import qualified LLVM.ExecutionEngine as EE

-- | Compile a Spinor expression to LLVM IR and JIT-execute it.
-- Returns the integer result or an error message.
runJIT :: Expr -> IO (Either Text Integer)
runJIT expr = do
  result <- try $ EE.simpleFunction (buildModule expr)
  case result of
    Left (e :: SomeException) ->
      return $ Left (T.pack $ "JIT error: " ++ show e)
    Right fn -> do
      n <- fn
      return $ Right (fromIntegral (n :: Int64))

-- | Build a CodeGenModule that defines a single function evaluating the expression.
buildModule :: Expr -> LC.CodeGenModule (LC.Function (IO Int64))
buildModule expr =
  LC.createFunction LC.ExternalLinkage (buildFunction expr)

-- | Build the function body that evaluates the expression and returns the result.
buildFunction :: Expr -> LC.CodeGenFunction Int64 ()
buildFunction expr = do
  result <- codegenExpr expr
  LC.ret result

-- | Generate LLVM IR for a single Spinor expression.
codegenExpr :: Expr -> LC.CodeGenFunction r (LC.Value Int64)
codegenExpr (EInt _ n) =
  return $ LC.valueOf (fromIntegral n :: Int64)

codegenExpr (EList _ [ESym _ "+", e1, e2]) = do
  v1 <- codegenExpr e1
  v2 <- codegenExpr e2
  LC.add v1 v2

codegenExpr (EList _ [ESym _ "-", e1, e2]) = do
  v1 <- codegenExpr e1
  v2 <- codegenExpr e2
  LC.sub v1 v2

codegenExpr (EList _ [ESym _ "*", e1, e2]) = do
  v1 <- codegenExpr e1
  v2 <- codegenExpr e2
  LC.mul v1 v2

codegenExpr _other =
  -- Unsupported expression: return 0
  return $ LC.valueOf (0 :: Int64)

#else

-- | Stub: LLVM is disabled in this build.
-- Build with @cabal build -f llvm@ to enable.
runJIT :: Expr -> IO (Either Text Integer)
runJIT _ = return $ Left "LLVM is disabled in this build. Use: cabal build -f llvm"

#endif
