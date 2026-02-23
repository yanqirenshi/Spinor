# Spec 49: WebGL Target for WASM (ブラウザでの可視化)

## 概要
Step 48 で導入した OpenGL プリミティブを、Emscripten 経由で WebGL/Canvas 上で動作させるためのトランスパイル・ビルド基盤を構築する。

## アーキテクチャ
ネイティブ環境では GLFW/OpenGL を使用するが、WASM (Emscripten) 環境では以下の代替ライブラリを使用する。
- **ウィンドウ/イベント管理:** SDL2 (Emscripten 内蔵)
- **描画:** OpenGL ES 2.0 (WebGL 1.0 相当)

### Emscripten Legacy GL 変換
初期実装では、トランスパイラ側でのコード変更を最小限にするため、Emscripten の `-s LEGACY_GL_EMULATION=1` フラグを活用する。これにより、固定機能パイプライン (`glBegin`, `glVertex`, `glEnd`) が WebGL のシェーダーベース描画に自動的にマッピングされる。

## トランスパイルルール (AST -> C)
`src/Spinor/Compiler/Codegen.hs` において、以下のマッピングを定義する。

| Spinor プリミティブ | 生成される C コード (WASM) | 備考 |
| :--- | :--- | :--- |
| `(gl-init w h title)` | `SDL_Init(SDL_INIT_VIDEO);` <br> `SDL_CreateWindow(title, ...)` | SDL2 を使用 |
| `(gl-clear)` | `glClear(GL_COLOR_BUFFER_BIT);` | |
| `(gl-draw-points m)` | `glBegin(GL_POINTS); ... glEnd();` | レガシーエミュレーションを使用 |
| `(gl-swap-buffers win)` | `SDL_GL_SwapWindow(win);` | |
| `(gl-window-should-close win)` | `SDL_PollEvent(&event); ...` | イベントループの監視 |

## ビルドパイプラインの拡張
`spinor build` コマンドに `--wasm` オプションを追加し、コンパイラを `gcc` から `emcc` に切り替える。

### emcc フラグ
- `-s USE_SDL=2`: SDL2 ライブラリの使用。
- `-s LEGACY_GL_EMULATION=1`: `glBegin`/`glEnd` のエミュレーション。
- `-o index.html`: WASM 実行用の HTML テンプレート生成。

## 考慮事項
- **メインループの制限:** ブラウザ (WASM) では無限ループはタブをフリーズさせるため、初期段階では簡易的なループで挙動を確認し、将来的に `emscripten_set_main_loop` への対応を検討する。
