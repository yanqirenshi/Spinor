# Spec 48: OpenGL Visualization (ネイティブ可視化基盤)

## 概要
Spinor に OpenGL と GLFW を用いたグラフィックス描画基盤を導入する。
Haskell の `GLFW-b` パッケージでウィンドウ管理と入力を、`OpenGL` パッケージで描画（頂点操作等）を行う。

## アーキテクチャ
Haskell インタプリタ側でウィンドウのライフサイクルと描画コマンドを制御する。
まずはシンプルかつ即時性のある「固定機能パイプライン (Immediate Mode)」に基づいた描画プリミティブを提供し、複雑なシェーダー操作は将来の拡張とする。

## データモデル
`src/Spinor/Val.hs` の `Val` 型に、GLFW ウィンドウを保持するための型を追加する。

```haskell
import qualified Graphics.UI.GLFW as GLFW

data Val
  = ...
  | VWindow GLFW.Window  -- GLFW ウィンドウハンドル
  | ...
```

## プリミティブ仕様

### 1. `(gl-init width height title)`
- **説明:** GLFW を初期化し、指定されたサイズとタイトルでウィンドウを作成する。
- **戻り値:** `VWindow`

### 2. `(gl-window-should-close win)`
- **説明:** ウィンドウの閉じるボタンが押されたか、または終了フラグがセットされているかを確認する。
- **戻り値:** `VBool` (`#t` または `#f`)

### 3. `(gl-swap-buffers win)`
- **説明:** フロントバッファとバックバッファを入れ替え、描画内容を画面に反映させる。同時に GLFW のイベント (`pollEvents`) を処理し、入力を更新する。
- **戻り値:** `VNil`

### 4. `(gl-clear)`
- **説明:** カラーバッファをクリアする（デフォルトは黒）。
- **戻り値:** `VNil`

### 5. `(gl-draw-points matrix)`
- **説明:** Nx2 または Nx3 の `VMatrix` を受け取り、各行を頂点座標として画面上に点 (GL_POINTS) を描画する。
- **引数:** `matrix`: `VMatrix`
- **動作:** `glBegin(GL_POINTS)`, `glVertex(...)`, `glEnd()` に相当する処理を実行する。
- **戻り値:** `VNil`

## 考慮事項
- **メインループ:** Spinor 側で `(while (not (gl-window-should-close win)) ...)` のようなループを記述して描画を行う。
- **座標系:** OpenGL 標準の正規化デバイス座標系 (-1.0 ～ 1.0) を使用する。
- **リソース管理:** ウィンドウの破棄 (`glfwDestroyWindow`) は、インタプリタ終了時または明示的な解放関数（将来実装）で行う。
