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
(GLFW の初期化手順や、VMatrix から頂点データへの変換効率化について)

### 実装内容
(変更したファイルの一覧、追加した主要な関数の説明など)
