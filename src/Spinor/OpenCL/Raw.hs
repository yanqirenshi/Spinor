{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

-- | OpenCL C API への最小限の FFI バインディング
--   GHC 9.6+ で動作しない Hackage の OpenCL パッケージに代わり、
--   必要な関数のみを直接バインドする。
module Spinor.OpenCL.Raw
  ( -- * 型
    CLPlatformID
  , CLDeviceID
  , CLContext
  , CLCommandQueue
  , CLMem
  , CLProgram
  , CLKernel
    -- * 定数
  , clDeviceTypeGPU
  , clDeviceTypeCPU
  , clMemReadWrite
  , clTrue
    -- * プラットフォーム / デバイス
  , clGetPlatformIDs
  , clGetDeviceIDs
    -- * コンテキスト / コマンドキュー
  , clCreateContext
  , clCreateCommandQueue
    -- * メモリ
  , clCreateBuffer
  , clEnqueueWriteBuffer
  , clEnqueueReadBuffer
  , clFinish
    -- * プログラム / カーネル
  , clCreateProgramWithSource
  , clBuildProgram
  , clGetProgramBuildLog
  , clCreateKernel
    -- * リソース解放
  , clReleaseProgram
    -- * コンテキスト情報
  , clGetContextDevices
  ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Data.Text (Text)
import qualified Data.Text as T

-- ===========================================================================
-- 型定義
-- ===========================================================================

type CLPlatformID   = Ptr ()
type CLDeviceID     = Ptr ()
type CLContext      = Ptr ()
type CLCommandQueue = Ptr ()
type CLMem          = Ptr ()
type CLProgram      = Ptr ()
type CLKernel       = Ptr ()

-- ===========================================================================
-- 定数
-- ===========================================================================

clDeviceTypeGPU :: CULLong
clDeviceTypeGPU = 4  -- CL_DEVICE_TYPE_GPU

clDeviceTypeCPU :: CULLong
clDeviceTypeCPU = 2  -- CL_DEVICE_TYPE_CPU

clMemReadWrite :: CULLong
clMemReadWrite = 1   -- CL_MEM_READ_WRITE

clTrue :: CUInt
clTrue = 1           -- CL_TRUE

-- ===========================================================================
-- FFI 宣言
-- ===========================================================================

-- Platform
foreign import ccall "clGetPlatformIDs"
  raw_clGetPlatformIDs :: CUInt -> Ptr CLPlatformID -> Ptr CUInt -> IO CInt

-- Device
foreign import ccall "clGetDeviceIDs"
  raw_clGetDeviceIDs :: CLPlatformID -> CULLong -> CUInt
                     -> Ptr CLDeviceID -> Ptr CUInt -> IO CInt

-- Context
foreign import ccall "clCreateContext"
  raw_clCreateContext :: Ptr CInt -> CUInt -> Ptr CLDeviceID
                      -> FunPtr () -> Ptr () -> Ptr CInt -> IO CLContext

-- CommandQueue (OpenCL 1.x)
foreign import ccall "clCreateCommandQueue"
  raw_clCreateCommandQueue :: CLContext -> CLDeviceID -> CULLong
                           -> Ptr CInt -> IO CLCommandQueue

-- Memory
foreign import ccall "clCreateBuffer"
  raw_clCreateBuffer :: CLContext -> CULLong -> CSize -> Ptr ()
                     -> Ptr CInt -> IO CLMem

foreign import ccall "clEnqueueWriteBuffer"
  raw_clEnqueueWriteBuffer :: CLCommandQueue -> CLMem -> CUInt
                           -> CSize -> CSize -> Ptr () -> CUInt
                           -> Ptr () -> Ptr () -> IO CInt

foreign import ccall "clEnqueueReadBuffer"
  raw_clEnqueueReadBuffer :: CLCommandQueue -> CLMem -> CUInt
                          -> CSize -> CSize -> Ptr () -> CUInt
                          -> Ptr () -> Ptr () -> IO CInt

foreign import ccall "clFinish"
  raw_clFinish :: CLCommandQueue -> IO CInt

-- Program
foreign import ccall "clCreateProgramWithSource"
  raw_clCreateProgramWithSource :: CLContext -> CUInt -> Ptr CString
                                -> Ptr CSize -> Ptr CInt -> IO CLProgram

foreign import ccall "clBuildProgram"
  raw_clBuildProgram :: CLProgram -> CUInt -> Ptr CLDeviceID
                     -> CString -> FunPtr () -> Ptr () -> IO CInt

foreign import ccall "clGetProgramBuildInfo"
  raw_clGetProgramBuildInfo :: CLProgram -> CLDeviceID -> CUInt
                            -> CSize -> Ptr () -> Ptr CSize -> IO CInt

-- Kernel
foreign import ccall "clCreateKernel"
  raw_clCreateKernel :: CLProgram -> CString -> Ptr CInt -> IO CLKernel

-- Release
foreign import ccall "clReleaseProgram"
  raw_clReleaseProgram :: CLProgram -> IO CInt

-- Context info
foreign import ccall "clGetContextInfo"
  raw_clGetContextInfo :: CLContext -> CUInt -> CSize -> Ptr ()
                       -> Ptr CSize -> IO CInt

-- ===========================================================================
-- Haskell ラッパー関数
-- ===========================================================================

-- | 利用可能なプラットフォームを取得
clGetPlatformIDs :: IO (Either Text [CLPlatformID])
clGetPlatformIDs = alloca $ \numPtr -> do
    err <- raw_clGetPlatformIDs 0 nullPtr numPtr
    if err /= 0
      then pure $ Left $ "clGetPlatformIDs failed: " <> tshow err
      else do
        num <- peek numPtr
        if num == 0
          then pure $ Right []
          else allocaArray (fromIntegral num) $ \buf -> do
            err2 <- raw_clGetPlatformIDs num buf numPtr
            if err2 /= 0
              then pure $ Left $ "clGetPlatformIDs failed: " <> tshow err2
              else Right <$> peekArray (fromIntegral num) buf

-- | 指定デバイスタイプのデバイスを取得
clGetDeviceIDs :: CLPlatformID -> CULLong -> IO (Either Text [CLDeviceID])
clGetDeviceIDs platform devType = alloca $ \numPtr -> do
    err <- raw_clGetDeviceIDs platform devType 0 nullPtr numPtr
    if err /= 0
      then pure $ Right []  -- デバイスが見つからない場合は空リスト
      else do
        num <- peek numPtr
        if num == 0
          then pure $ Right []
          else allocaArray (fromIntegral num) $ \buf -> do
            err2 <- raw_clGetDeviceIDs platform devType num buf numPtr
            if err2 /= 0
              then pure $ Left $ "clGetDeviceIDs failed: " <> tshow err2
              else Right <$> peekArray (fromIntegral num) buf

-- | コンテキストを作成
clCreateContext :: [CLDeviceID] -> IO (Either Text CLContext)
clCreateContext devices =
    withArrayLen devices $ \len devArr -> alloca $ \errPtr -> do
      ctx <- raw_clCreateContext nullPtr (fromIntegral len) devArr
                                 nullFunPtr nullPtr errPtr
      err <- peek errPtr
      if err /= 0
        then pure $ Left $ "clCreateContext failed: " <> tshow err
        else pure $ Right ctx

-- | コマンドキューを作成
clCreateCommandQueue :: CLContext -> CLDeviceID -> IO (Either Text CLCommandQueue)
clCreateCommandQueue ctx dev = alloca $ \errPtr -> do
    queue <- raw_clCreateCommandQueue ctx dev 0 errPtr
    err <- peek errPtr
    if err /= 0
      then pure $ Left $ "clCreateCommandQueue failed: " <> tshow err
      else pure $ Right queue

-- | バッファを作成
clCreateBuffer :: CLContext -> CULLong -> Int -> IO (Either Text CLMem)
clCreateBuffer ctx flags byteSize = alloca $ \errPtr -> do
    mem <- raw_clCreateBuffer ctx flags (fromIntegral byteSize) nullPtr errPtr
    err <- peek errPtr
    if err /= 0
      then pure $ Left $ "clCreateBuffer failed: " <> tshow err
      else pure $ Right mem

-- | ホスト→デバイスのデータ転送 (ブロッキング)
clEnqueueWriteBuffer :: CLCommandQueue -> CLMem -> Int -> Ptr () -> IO (Either Text ())
clEnqueueWriteBuffer queue mem byteSize srcPtr = do
    err <- raw_clEnqueueWriteBuffer queue mem clTrue 0
             (fromIntegral byteSize) srcPtr 0 nullPtr nullPtr
    if err /= 0
      then pure $ Left $ "clEnqueueWriteBuffer failed: " <> tshow err
      else pure $ Right ()

-- | デバイス→ホストのデータ読み戻し (ブロッキング)
clEnqueueReadBuffer :: CLCommandQueue -> CLMem -> Int -> Ptr () -> IO (Either Text ())
clEnqueueReadBuffer queue mem byteSize dstPtr = do
    err <- raw_clEnqueueReadBuffer queue mem clTrue 0
             (fromIntegral byteSize) dstPtr 0 nullPtr nullPtr
    if err /= 0
      then pure $ Left $ "clEnqueueReadBuffer failed: " <> tshow err
      else pure $ Right ()

-- | キューの完了を待つ
clFinish :: CLCommandQueue -> IO (Either Text ())
clFinish queue = do
    err <- raw_clFinish queue
    if err /= 0
      then pure $ Left $ "clFinish failed: " <> tshow err
      else pure $ Right ()

-- | ソースコードからプログラムを作成
clCreateProgramWithSource :: CLContext -> String -> IO (Either Text CLProgram)
clCreateProgramWithSource ctx source =
    withCString source $ \cstr ->
      with cstr $ \strPtr ->
        alloca $ \errPtr -> do
          prog <- raw_clCreateProgramWithSource ctx 1 strPtr nullPtr errPtr
          err <- peek errPtr
          if err /= 0
            then pure $ Left $ "clCreateProgramWithSource failed: " <> tshow err
            else pure $ Right prog

-- | プログラムをビルド
clBuildProgram :: CLProgram -> [CLDeviceID] -> IO (Either Text ())
clBuildProgram prog devices =
    withArrayLen devices $ \len devArr ->
      withCString "" $ \opts -> do
        err <- raw_clBuildProgram prog (fromIntegral len) devArr opts nullFunPtr nullPtr
        if err /= 0
          then pure $ Left $ "clBuildProgram failed: " <> tshow err
          else pure $ Right ()

-- | ビルドログを取得
clGetProgramBuildLog :: CLProgram -> CLDeviceID -> IO Text
clGetProgramBuildLog prog dev = alloca $ \sizePtr -> do
    -- CL_PROGRAM_BUILD_LOG = 0x1183
    _ <- raw_clGetProgramBuildInfo prog dev 0x1183 0 nullPtr sizePtr
    logSize <- peek sizePtr
    if logSize <= 1
      then pure ""
      else allocaBytes (fromIntegral logSize) $ \buf -> do
        _ <- raw_clGetProgramBuildInfo prog dev 0x1183 logSize buf sizePtr
        T.pack <$> peekCString (castPtr buf)

-- | カーネルを作成
clCreateKernel :: CLProgram -> String -> IO (Either Text CLKernel)
clCreateKernel prog name =
    withCString name $ \cstr -> alloca $ \errPtr -> do
      kernel <- raw_clCreateKernel prog cstr errPtr
      err <- peek errPtr
      if err /= 0
        then pure $ Left $ "clCreateKernel failed: " <> tshow err
        else pure $ Right kernel

-- | プログラムを解放
clReleaseProgram :: CLProgram -> IO ()
clReleaseProgram prog = do
    _ <- raw_clReleaseProgram prog
    pure ()

-- | コンテキストに関連付けられたデバイスを取得
clGetContextDevices :: CLContext -> IO (Either Text [CLDeviceID])
clGetContextDevices ctx = alloca $ \sizePtr -> do
    -- CL_CONTEXT_DEVICES = 0x1081
    err <- raw_clGetContextInfo ctx 0x1081 0 nullPtr sizePtr
    if err /= 0
      then pure $ Left $ "clGetContextInfo failed: " <> tshow err
      else do
        byteSize <- peek sizePtr
        let devCount = fromIntegral byteSize `div` sizeOf (undefined :: CLDeviceID)
        allocaArray devCount $ \buf -> do
          err2 <- raw_clGetContextInfo ctx 0x1081 byteSize (castPtr buf) sizePtr
          if err2 /= 0
            then pure $ Left $ "clGetContextInfo failed: " <> tshow err2
            else Right <$> peekArray devCount buf

-- ===========================================================================
-- ヘルパー
-- ===========================================================================

tshow :: Show a => a -> Text
tshow = T.pack . show
