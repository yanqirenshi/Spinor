# Task 49: WebGL Target for WASM (ブラウザでの可視化) の実装

OpenGL プリミティブを WASM/WebGL 環境にトランスパイル・ビルドするための機能を実装してください。

## ステップ

### 1. トランスパイラの拡張 (Codegen.hs)
- `src/Spinor/Compiler/Codegen.hs` を修正し、OpenGL プリミティブに対する C コード生成ロジックを追加してください。
- 出力される C コードの先頭に、`__EMSCRIPTEN__` マクロで分岐するインクルード定義を追加してください。
  ```c
  #ifdef __EMSCRIPTEN__
  #include <SDL2/SDL.h>
  #include <GLES2/gl2.h>
  #else
  #include <GLFW/glfw3.h>
  #include <GL/gl.h>
  #endif
  ```

### 2. ビルドコマンドの修正 (Main.hs)
- `app/Main.hs` の `buildMode` 関数を修正し、`--wasm` フラグを解釈するようにしてください。
- `emcc` が PATH に通っているか確認する `findEmcc` ヘルパーを実装してください。
- `--wasm` 指定時、`emcc` に対して以下のフラグを渡してビルドするようにしてください。
  `-s USE_SDL=2 -s LEGACY_GL_EMULATION=1 -o <file>.html`

### 3. 動作確認
簡単な点描画スクリプトをビルドし、ブラウザで表示を確認してください。

## 実装報告ルール
実装完了後、**このファイル自体を編集して**、以下のセクションを末尾に追記してください。

### 実装方針

**トランスパイラの拡張方針:**
- 生成される C コードの先頭に `#ifdef __EMSCRIPTEN__` で分岐するインクルードブロック (`glIncludes`) を挿入
- WASM 環境では SDL2/GLES2、ネイティブ環境では GLFW/GL を使用するヘルパー関数群 (`glHelpers`) をインラインで生成
- ヘルパー関数はグローバル変数 (`_sp_gl_window` 等) でウィンドウハンドルを管理し、SpObject* インターフェースでラップ
- `compileExpr` に 5 つの GL プリミティブのパターンマッチを追加し、各ヘルパー関数呼び出しに変換
- `primitives` リストに GL プリミティブ名を追加し、ユーザー定義関数として誤処理されないようガード

**ビルドコマンドの拡張方針:**
- `spinor build <file> --wasm` (順序不問) で WASM ビルドを実行
- `findEmcc` で emcc の PATH 検索を行い、見つからない場合は警告付きでフォールバック
- emcc フラグ: `-s USE_SDL=2 -s LEGACY_GL_EMULATION=1 -o <base>.html`
- 出力は `.html` (Emscripten が .html, .js, .wasm の 3 ファイルを生成)

### 実装内容

**変更ファイル一覧:**

1. **src/Spinor/Compiler/Codegen.hs**
   - `glIncludes :: CCode`: `#ifdef __EMSCRIPTEN__` による SDL2/GLES2 と GLFW/GL の分岐インクルード生成
   - `glHelpers :: CCode`: 5 つの C ヘルパー関数をインラインで生成
     - `sp_gl_init(w, h, title)`: SDL2 / GLFW によるウィンドウ初期化
     - `sp_gl_clear()`: `glClear(GL_COLOR_BUFFER_BIT)`
     - `sp_gl_swap_buffers(win)`: SDL2 / GLFW によるバッファスワップ
     - `sp_gl_window_should_close(win)`: SDL2 イベントポーリング / GLFW フラグ確認
     - `sp_gl_draw_points(data)`: `glBegin(GL_POINTS)` による描画 (頂点データ展開は TODO)
   - `compileExpr`: 5 つの GL プリミティブのパターンマッチを追加
   - `primitives` リストに `gl-init`, `gl-clear`, `gl-draw-points`, `gl-swap-buffers`, `gl-window-should-close` を追加
   - `compileProgram`: 出力の先頭に `glIncludes` と `glHelpers` を挿入

2. **app/Main.hs**
   - `buildWasmMode :: FilePath -> IO ()`: WASM ビルドモード (emcc 呼び出し)
   - `findEmcc :: IO FilePath`: emcc の PATH 検索ヘルパー
   - コマンドラインパターンマッチに `["build", file, "--wasm"]` と `["build", "--wasm", file]` を追加
   - ヘルプメッセージに `build <file> --wasm` の説明を追加

**動作確認:**
- `cabal build`: 警告なしでビルド成功
- `cabal test`: 全 152 テストパス (0 failures)
- ※WASM ビルドの実行テスト (emcc による .html 生成) は Emscripten インストール環境で別途実施が必要
