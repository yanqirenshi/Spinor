{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

-- | LLVM Backend for Spinor
--
-- This module provides LLVM IR generation and JIT execution capabilities.
-- Currently supports only integer literals, addition, and multiplication.
module Spinor.Compiler.LLVM
  ( codegen
  , runJIT
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Spinor.Syntax (Expr(..))

-- Import LLVM pure (for IR generation)
import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as IP
import LLVM.AST.Name (Name(..))
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction

-- Import LLVM (for JIT execution)
import LLVM.Context
import LLVM.Module
import LLVM.Target
import LLVM.ExecutionEngine

import Foreign.Ptr (FunPtr, castFunPtr)

-- | Generate LLVM IR Module from a Spinor expression
codegen :: Expr -> AST.Module
codegen expr = buildModule "spinor_jit" $ do
  -- Define main function that evaluates the expression and returns result
  function "main" [] AST.i64 $ \[] -> mdo
    result <- codegenExpr expr
    ret result

-- | Generate LLVM IR for a single expression
codegenExpr :: (MonadIRBuilder m) => Expr -> m AST.Operand
codegenExpr (EInt _ n) =
  pure $ AST.ConstantOperand $ C.Int 64 n

codegenExpr (EList _ [ESym _ "+", e1, e2]) = do
  v1 <- codegenExpr e1
  v2 <- codegenExpr e2
  add v1 v2

codegenExpr (EList _ [ESym _ "*", e1, e2]) = do
  v1 <- codegenExpr e1
  v2 <- codegenExpr e2
  mul v1 v2

codegenExpr (EList _ [ESym _ "-", e1, e2]) = do
  v1 <- codegenExpr e1
  v2 <- codegenExpr e2
  sub v1 v2

codegenExpr other =
  -- For unsupported expressions, return 0
  pure $ AST.ConstantOperand $ C.Int 64 0

-- | Foreign import for calling the JIT-compiled function
foreign import ccall "dynamic"
  mkMain :: FunPtr (IO Int) -> IO Int

-- | Execute LLVM IR via JIT and return the result
runJIT :: AST.Module -> IO (Either Text Integer)
runJIT llvmModule = do
  withContext $ \ctx ->
    withModuleFromAST ctx llvmModule $ \m -> do
      withHostTargetMachineDefault $ \tm ->
        withExecutionEngine tm m $ \ee -> do
          maybeFn <- getFunction ee (Name "main")
          case maybeFn of
            Nothing -> return $ Left "Failed to find main function"
            Just fn -> do
              result <- mkMain (castFunPtr fn)
              return $ Right (fromIntegral result)
