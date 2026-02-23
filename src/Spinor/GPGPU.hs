{-# LANGUAGE OverloadedStrings #-}

module Spinor.GPGPU
  ( gpgpuBindings
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Storable as VS
import Control.Exception (SomeException, try)
import Foreign.Ptr (castPtr)
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Storable (sizeOf, peekElemOff)
import System.IO.Unsafe (unsafePerformIO)

import qualified Spinor.OpenCL.Raw as CL
import Spinor.Val (Val(..), Env)

-- | OpenCL 関連プリミティブの束縛
gpgpuBindings :: Env
gpgpuBindings = Map.fromList
  [ ("cl-init",    VPrim "cl-init"    primCLInit)
  , ("to-device",  VPrim "to-device"  primToDevice)
  , ("to-host",    VPrim "to-host"    primToHost)
  , ("cl-compile", VPrim "cl-compile" primCLCompile)
  ]

-- ===========================================================================
-- OpenCL プリミティブ実装
-- ===========================================================================

-- | cl-init: OpenCL の初期化
--   GPU を優先し、なければ CPU にフォールバック
--   戻り値: VCLContext (Context, CommandQueue)
primCLInit :: [Val] -> Either Text Val
primCLInit [] = unsafeIOToEither $ do
    platformResult <- CL.clGetPlatformIDs
    case platformResult of
      Left err -> pure $ Left $ "cl-init: " <> err
      Right [] -> pure $ Left "cl-init: OpenCL プラットフォームが見つかりません"
      Right (platform:_) -> do
        device <- pickDevice platform
        case device of
          Nothing -> pure $ Left "cl-init: 利用可能な OpenCL デバイスが見つかりません"
          Just dev -> do
            ctxResult <- CL.clCreateContext [dev]
            case ctxResult of
              Left err -> pure $ Left $ "cl-init: " <> err
              Right ctx -> do
                queueResult <- CL.clCreateCommandQueue ctx dev
                case queueResult of
                  Left err -> pure $ Left $ "cl-init: " <> err
                  Right queue -> pure $ Right $ VCLContext ctx queue
  where
    pickDevice :: CL.CLPlatformID -> IO (Maybe CL.CLDeviceID)
    pickDevice platform = do
        gpuResult <- CL.clGetDeviceIDs platform CL.clDeviceTypeGPU
        case gpuResult of
          Right (d:_) -> pure (Just d)
          _ -> do
            cpuResult <- CL.clGetDeviceIDs platform CL.clDeviceTypeCPU
            case cpuResult of
              Right (d:_) -> pure (Just d)
              _ -> pure Nothing
primCLInit args = Left $ "cl-init: 引数の数が不正です (期待: 0, 実際: "
                       <> tshow (length args) <> ")"

-- | to-device: VMatrix のデータを GPU バッファに転送
--   (to-device ctx matrix) -> VCLBuffer
primToDevice :: [Val] -> Either Text Val
primToDevice [VCLContext ctx queue, VMatrix _rows _cols vec] = unsafeIOToEither $ do
    let elemCount = VS.length vec
        byteSize  = elemCount * sizeOf (undefined :: Double)
    bufResult <- CL.clCreateBuffer ctx CL.clMemReadWrite byteSize
    case bufResult of
      Left err -> pure $ Left $ "to-device: " <> err
      Right mem -> do
        VS.unsafeWith vec $ \srcPtr -> do
          writeResult <- CL.clEnqueueWriteBuffer queue mem byteSize (castPtr srcPtr)
          case writeResult of
            Left err -> pure $ Left $ "to-device: " <> err
            Right () -> do
              finResult <- CL.clFinish queue
              case finResult of
                Left err -> pure $ Left $ "to-device: " <> err
                Right () -> pure $ Right $ VCLBuffer mem elemCount
primToDevice [VCLContext{}, _] = Left "to-device: 第2引数に行列 (VMatrix) が必要です"
primToDevice [_, _]            = Left "to-device: 第1引数に CLContext が必要です"
primToDevice args = Left $ "to-device: 引数の数が不正です (期待: 2, 実際: "
                         <> tshow (length args) <> ")"

-- | to-host: GPU バッファのデータを CPU に読み戻して VMatrix を生成
--   (to-host ctx buffer rows cols) -> VMatrix
primToHost :: [Val] -> Either Text Val
primToHost [VCLContext _ctx queue, VCLBuffer mem bufSize, VInt rows, VInt cols] = do
    let expectedElems = fromIntegral (rows * cols) :: Int
    if expectedElems /= bufSize
      then Left $ "to-host: バッファサイズと次元が不一致です (バッファ: "
                <> tshow bufSize <> " 要素, 指定: "
                <> tshow rows <> "x" <> tshow cols <> "="
                <> tshow expectedElems <> " 要素)"
      else unsafeIOToEither $ do
        let byteSize = bufSize * sizeOf (undefined :: Double)
        buf <- mallocBytes byteSize
        readResult <- CL.clEnqueueReadBuffer queue mem byteSize (castPtr buf)
        case readResult of
          Left err -> do
            free buf
            pure $ Left $ "to-host: " <> err
          Right () -> do
            finResult <- CL.clFinish queue
            case finResult of
              Left err -> do
                free buf
                pure $ Left $ "to-host: " <> err
              Right () -> do
                vec <- VS.generateM bufSize (\i -> peekElemOff buf i)
                free buf
                pure $ Right $ VMatrix (fromIntegral rows) (fromIntegral cols) vec
primToHost [VCLContext{}, VCLBuffer{}, _, _] = Left "to-host: 第3,4引数に整数 (rows, cols) が必要です"
primToHost [VCLContext{}, _, _, _]           = Left "to-host: 第2引数に CLBuffer が必要です"
primToHost [_, _, _, _]                      = Left "to-host: 第1引数に CLContext が必要です"
primToHost args = Left $ "to-host: 引数の数が不正です (期待: 4, 実際: "
                       <> tshow (length args) <> ")"

-- | cl-compile: OpenCL カーネルソースをコンパイル
--   (cl-compile ctx source kernel-name) -> VCLKernel
primCLCompile :: [Val] -> Either Text Val
primCLCompile [VCLContext ctx _queue, VStr source, VStr kernelName] = unsafeIOToEither $ do
    devResult <- CL.clGetContextDevices ctx
    case devResult of
      Left err -> pure $ Left $ "cl-compile: " <> err
      Right [] -> pure $ Left "cl-compile: コンテキストにデバイスが関連付けられていません"
      Right devices -> do
        progResult <- CL.clCreateProgramWithSource ctx (T.unpack source)
        case progResult of
          Left err -> pure $ Left $ "cl-compile: " <> err
          Right program -> do
            buildResult <- CL.clBuildProgram program devices
            case buildResult of
              Left _ -> do
                -- ビルドログ取得
                logs <- mapM (CL.clGetProgramBuildLog program) devices
                let combinedLog = T.intercalate "\n" logs
                CL.clReleaseProgram program
                pure $ Left $ "cl-compile: カーネルのコンパイルに失敗しました\n" <> combinedLog
              Right () -> do
                kernelResult <- CL.clCreateKernel program (T.unpack kernelName)
                case kernelResult of
                  Left err -> do
                    CL.clReleaseProgram program
                    pure $ Left $ "cl-compile: カーネル '" <> kernelName
                                <> "' の取得に失敗しました: " <> err
                  Right kernel ->
                    pure $ Right $ VCLKernel kernel kernelName
primCLCompile [VCLContext{}, VStr{}, _] = Left "cl-compile: 第3引数にカーネル名 (文字列) が必要です"
primCLCompile [VCLContext{}, _, _]      = Left "cl-compile: 第2引数にソースコード (文字列) が必要です"
primCLCompile [_, _, _]                 = Left "cl-compile: 第1引数に CLContext が必要です"
primCLCompile args = Left $ "cl-compile: 引数の数が不正です (期待: 3, 実際: "
                          <> tshow (length args) <> ")"

-- ===========================================================================
-- ヘルパー関数
-- ===========================================================================

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | IO アクションを Either に変換 (OpenCL 例外をキャッチ)
unsafeIOToEither :: IO (Either Text Val) -> Either Text Val
unsafeIOToEither action = case unsafePerformIOCatch action of
    Left e  -> Left $ "OpenCL エラー: " <> T.pack (show e)
    Right r -> r

-- | IO を実行して例外をキャッチ
unsafePerformIOCatch :: IO a -> Either SomeException a
unsafePerformIOCatch action =
    unsafePerformIO (try action)
