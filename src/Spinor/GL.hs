{-# LANGUAGE OverloadedStrings #-}

module Spinor.GL
  ( glBindings
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Storable as VS
import Control.Exception (SomeException, try)
import System.IO.Unsafe (unsafePerformIO)

import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL
  ( clear, ClearBuffer(..), renderPrimitive, PrimitiveMode(..)
  , vertex, Vertex2(..), Vertex3(..), GLdouble
  )

import Spinor.Val (Val(..), Env)

-- | OpenGL 関連プリミティブの束縛
glBindings :: Env
glBindings = Map.fromList
  [ ("gl-init",                VPrim "gl-init"                primGLInit)
  , ("gl-window-should-close", VPrim "gl-window-should-close" primGLWindowShouldClose)
  , ("gl-swap-buffers",        VPrim "gl-swap-buffers"        primGLSwapBuffers)
  , ("gl-clear",               VPrim "gl-clear"               primGLClear)
  , ("gl-draw-points",         VPrim "gl-draw-points"         primGLDrawPoints)
  ]

-- ===========================================================================
-- OpenGL プリミティブ実装
-- ===========================================================================

-- | gl-init: GLFW を初期化し、ウィンドウを作成する
--   (gl-init width height title) -> VWindow
primGLInit :: [Val] -> Either Text Val
primGLInit [VInt w, VInt h, VStr title] = unsafeIOToEither $ do
    ok <- GLFW.init
    if not ok
      then pure $ Left "gl-init: GLFW の初期化に失敗しました"
      else do
        mWin <- GLFW.createWindow (fromIntegral w) (fromIntegral h)
                                  (T.unpack title) Nothing Nothing
        case mWin of
          Nothing -> do
            GLFW.terminate
            pure $ Left "gl-init: ウィンドウの作成に失敗しました"
          Just win -> do
            GLFW.makeContextCurrent (Just win)
            pure $ Right $ VWindow win
primGLInit [_, _, _] = Left "gl-init: (Int, Int, String) が必要です"
primGLInit args = Left $ "gl-init: 引数の数が不正です (期待: 3, 実際: "
                       <> tshow (length args) <> ")"

-- | gl-window-should-close: ウィンドウが閉じられるべきかを判定
--   (gl-window-should-close win) -> VBool
primGLWindowShouldClose :: [Val] -> Either Text Val
primGLWindowShouldClose [VWindow win] = unsafeIOToEither $ do
    shouldClose <- GLFW.windowShouldClose win
    pure $ Right $ VBool shouldClose
primGLWindowShouldClose [_] = Left "gl-window-should-close: Window が必要です"
primGLWindowShouldClose args = Left $ "gl-window-should-close: 引数の数が不正です (期待: 1, 実際: "
                                    <> tshow (length args) <> ")"

-- | gl-swap-buffers: バッファを交換し、イベントをポーリングする
--   (gl-swap-buffers win) -> VNil
primGLSwapBuffers :: [Val] -> Either Text Val
primGLSwapBuffers [VWindow win] = unsafeIOToEither $ do
    GLFW.swapBuffers win
    GLFW.pollEvents
    pure $ Right VNil
primGLSwapBuffers [_] = Left "gl-swap-buffers: Window が必要です"
primGLSwapBuffers args = Left $ "gl-swap-buffers: 引数の数が不正です (期待: 1, 実際: "
                              <> tshow (length args) <> ")"

-- | gl-clear: カラーバッファをクリアする
--   (gl-clear) -> VNil
primGLClear :: [Val] -> Either Text Val
primGLClear [] = unsafeIOToEither $ do
    clear [ColorBuffer]
    pure $ Right VNil
primGLClear args = Left $ "gl-clear: 引数の数が不正です (期待: 0, 実際: "
                        <> tshow (length args) <> ")"

-- | gl-draw-points: VMatrix の各行を頂点として点を描画する
--   (gl-draw-points matrix) -> VNil
--   Nx2 の場合は Vertex2、Nx3 の場合は Vertex3 として描画
primGLDrawPoints :: [Val] -> Either Text Val
primGLDrawPoints [VMatrix rows cols vec]
    | cols == 2 = unsafeIOToEither $ do
        renderPrimitive Points $ do
          let go r = do
                let x = realToFrac (vec VS.! (r * 2))     :: GLdouble
                    y = realToFrac (vec VS.! (r * 2 + 1)) :: GLdouble
                vertex (Vertex2 x y)
          mapM_ go [0 .. rows - 1]
        pure $ Right VNil
    | cols == 3 = unsafeIOToEither $ do
        renderPrimitive Points $ do
          let go r = do
                let x = realToFrac (vec VS.! (r * 3))     :: GLdouble
                    y = realToFrac (vec VS.! (r * 3 + 1)) :: GLdouble
                    z = realToFrac (vec VS.! (r * 3 + 2)) :: GLdouble
                vertex (Vertex3 x y z)
          mapM_ go [0 .. rows - 1]
        pure $ Right VNil
    | otherwise = Left $ "gl-draw-points: 列数は 2 または 3 である必要があります (実際: "
                       <> tshow cols <> ")"
primGLDrawPoints [_] = Left "gl-draw-points: Matrix が必要です"
primGLDrawPoints args = Left $ "gl-draw-points: 引数の数が不正です (期待: 1, 実際: "
                             <> tshow (length args) <> ")"

-- ===========================================================================
-- ヘルパー関数
-- ===========================================================================

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | IO アクションを Either に変換 (例外をキャッチ)
unsafeIOToEither :: IO (Either Text Val) -> Either Text Val
unsafeIOToEither action = case unsafePerformIOCatch action of
    Left e  -> Left $ "GL エラー: " <> T.pack (show e)
    Right r -> r

-- | IO を実行して例外をキャッチ
unsafePerformIOCatch :: IO a -> Either SomeException a
unsafePerformIOCatch action =
    unsafePerformIO (try action)
