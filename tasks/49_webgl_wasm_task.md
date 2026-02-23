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
(Emscripten のビルドオプション設定や、トランスパイル時の工夫について)

### 実装内容
(変更したファイルの一覧、追加した主要な関数の説明など)
