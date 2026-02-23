# Task 48: OpenGL Visualization (ネイティブ可視化基盤) の実装

ウィンドウ作成と基本的な点描画を行う OpenGL プリミティブを実装してください。

## ステップ

### 1. 依存関係の追加
- `spinor.cabal` の `library` セクションに `GLFW-b` と `OpenGL` を追加してください。
- ※ Linux 環境では `libglfw3-dev`, `libgl1-mesa-dev` などのインストールが必要になる場合があります。

### 2. データ型の追加 (Val.hs)
- `src/Spinor/Val.hs` の `Val` 型に `VWindow GLFW.Window` を追加してください。
- `showVal` を修正し、`"<Window>"` と表示するようにしてください。
- `Eq` インスタンスも適切に更新してください（ウィンドウハンドルはポインタ比較）。

### 3. OpenGL プリミティブの実装
- `src/Spinor/GL.hs` を新規作成し、以下のプリミティブを実装してください。
    - `primGLInit`: `GLFW.init`, `GLFW.createWindow`, `GLFW.makeContextCurrent` を実行。
    - `primGLWindowShouldClose`: `GLFW.windowShouldClose` をラップ。
    - `primGLSwapBuffers`: `GLFW.swapBuffers` と `GLFW.pollEvents` を実行。
    - `primGLClear`: `glClear` を実行。
    - `primGLDrawPoints`: `VMatrix` をパースし、`renderPrimitive Points` 内で `vertex (Vertex2 x y)` を呼び出す。
- `src/Spinor/Primitive.hs` で `glBindings` を作成し、`primitiveBindings` に `Map.union` してください。

### 4. ドキュメントの更新 (Docs.hs)
- `src/Spinor/Lsp/Docs.hs` に OpenGL 関連の 5 つの関数のドキュメントを追加してください。

### 5. 検証用コードの作成
REPL またはファイルで以下のコードを実行し、ウィンドウが開いて点が描画されるか手動検証してください。

```lisp
(def win (gl-init 640 480 "Spinor GL Test"))

(def pts (matrix 100 2 (list-of-random-doubles 200))) ; -1.0〜1.0の範囲を想定

(def loop (fn ()
  (if (not (gl-window-should-close win))
      (begin
        (gl-clear)
        (gl-draw-points pts)
        (gl-swap-buffers win)
        (loop))
      (print "Window closed."))))

(loop)
```

## 実装報告ルール
実装完了後、**このファイル自体を編集して**、以下のセクションを末尾に追記してください。

### 実装方針

**GLFW 初期化手順:**
- `primGLInit` で `GLFW.init` → `GLFW.createWindow` → `GLFW.makeContextCurrent` の順に実行
- GLFW の初期化失敗時は `GLFW.terminate` を呼ばず即座にエラーを返す
- ウィンドウ作成失敗時は `GLFW.terminate` で後始末してからエラーを返す

**VMatrix から頂点データへの変換:**
- `VMatrix` の `Data.Vector.Storable` を直接インデックスアクセスし、各行を `Vertex2 x y` (Nx2) または `Vertex3 x y z` (Nx3) に変換
- `renderPrimitive Points` ブロック内で `mapM_` による行ループで頂点を順次描画
- 新たなメモリ確保は不要で、元の Vector からの読み出しのみ

**IO と純粋関数の橋渡し:**
- OpenCL プリミティブと同様に `unsafePerformIO` + `try` で IO アクションを `Either Text Val` に変換
- GLFW/OpenGL の各操作は IO モナド内で実行

**型環境の更新:**
- Task 47 での教訓を活かし、`baseTypeEnv` に OpenGL プリミティブの型シグネチャも同時に追加
- REPL で `gl-init` 等が型推論エラーにならないよう対応済み

### 実装内容

**変更ファイル一覧:**

1. **spinor.cabal**
   - `GLFW-b >= 3.3 && < 4` と `OpenGL >= 3.0 && < 4` を `build-depends` に追加
   - `Spinor.GL` を `exposed-modules` に追加

2. **src/Spinor/Val.hs**
   - `VWindow GLFW.Window` を `Val` 型に追加
   - `import qualified Graphics.UI.GLFW as GLFW` を追加
   - `showVal (VWindow _) = "<Window>"` を追加
   - `Eq` インスタンスに `VWindow w1 == VWindow w2 = w1 == w2` を追加 (GLFW.Window は Eq インスタンスを持つ)

3. **src/Spinor/GL.hs** (新規作成)
   - `glBindings`: OpenGL プリミティブの環境辞書
   - `primGLInit` (`gl-init`): GLFW 初期化 + ウィンドウ作成 + OpenGL コンテキスト設定
   - `primGLWindowShouldClose` (`gl-window-should-close`): ウィンドウ閉じフラグの確認
   - `primGLSwapBuffers` (`gl-swap-buffers`): バッファスワップ + イベントポーリング
   - `primGLClear` (`gl-clear`): カラーバッファのクリア
   - `primGLDrawPoints` (`gl-draw-points`): Nx2/Nx3 行列から GL_POINTS 描画

4. **src/Spinor/Primitive.hs**
   - `Spinor.GL (glBindings)` をインポートし、`Map.unions [corePrimitives, gpgpuBindings, glBindings]` で統合

5. **src/Spinor/Server.hs**
   - `formatValForDisassembly`, `valContentText`, `valTitle`, `valTypeName` に `VWindow` のパターンマッチを追加

6. **src/Spinor/Infer.hs**
   - `baseTypeEnv` に `gl-init`, `gl-window-should-close`, `gl-swap-buffers`, `gl-clear`, `gl-draw-points` の型シグネチャを追加

7. **src/Spinor/Lsp/Docs.hs**
   - 5 つの OpenGL プリミティブ (`gl-init`, `gl-window-should-close`, `gl-swap-buffers`, `gl-clear`, `gl-draw-points`) の CLHS 形式ドキュメントを追加

**システム依存関係 (WSL2/Linux):**
- `libglfw3-dev`, `libgl1-mesa-dev` (事前インストール済み)
- `libxxf86vm-dev`, `libxrandr-dev`, `libxinerama-dev`, `libxcursor-dev`, `libxi-dev` (bindings-GLFW のビルドに必要)

**動作確認 (WSL2 環境):**
- `cabal build`: 警告なしでビルド成功
- `cabal test`: 全 152 テストパス (0 failures)
- ※OpenGL ランタイムテスト (ウィンドウ表示・点描画) は X11/Wayland ディスプレイ環境で別途実施が必要
