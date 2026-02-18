# Step 26 実装ハンドオーバー

## 概要

Step 26「スタンドアロン コンパイル」の実装は完了しています。WSL2 (Ubuntu) 環境での動作確認が必要です。

## 実装済みの変更

### 1. `src/Spinor/Compiler/Codegen.hs`

以下の機能を追加:

- **`compileProgram`**: `defun` 式とその他の式を `partition` で分類し、それぞれ C 関数定義と main 関数内の文に変換
- **`isDefun`**: `defun` 式かどうかを判定
- **`compileFunDef`**: `(defun name (args) body)` を C 関数定義に変換
- **`mangle`**: Spinor シンボルを C 識別子に変換 (`add-100` → `user_add_100`)
- **`compileExpr` の拡張**:
  - 変数参照 (`ESym`) → マングリングされた識別子
  - ユーザー定義関数呼び出し → `user_xxx(...)` 形式

### 2. `app/Main.hs`

以下の機能を追加:

- **`build` サブコマンド**: `spinor build <file.spin>` でネイティブバイナリを生成
- **`buildMode`**: C コード生成 → gcc 呼び出し → クリーンアップ
- **`findGcc`**: gcc を自動検出 (PATH, MSYS2 UCRT, MSYS2 MINGW64 の順)
- **`-Iruntime` オプション**: runtime ディレクトリをインクルードパスに追加

### 3. `spinor.cabal`

- `process` パッケージを executable の依存関係に追加

## 生成される C コード例

入力 (`test-build.spin`):
```lisp
(defun add-100 (n)
  (+ n 100))

(add-100 42)
```

出力 (`test-build.c`):
```c
#include <stdio.h>
#include <stdbool.h>
#include "spinor.h"

SpObject* user_add_100(SpObject* user_n) {
    return sp_add(user_n, sp_make_int(100));
}

int main(void) {
    sp_print(user_add_100(sp_make_int(42)));

    return 0;
}
```

## WSL2 での確認手順

1. **ビルド確認**:
   ```bash
   cd /path/to/Spinor
   cabal build
   ```

2. **テストファイル作成** (存在しない場合):
   ```bash
   cat > test-build.spin << 'EOF'
   ; test-build.spin

   (defun add-100 (n)
     (+ n 100))

   (add-100 42)
   EOF
   ```

3. **build コマンド実行**:
   ```bash
   cabal run spinor -- build test-build.spin
   ```

4. **期待される出力**:
   ```
   Compiling test-build.spin to test-build.c...
   Building test-build with gcc...
   Build successful. Executable created: test-build
   ```

5. **生成されたバイナリの実行**:
   ```bash
   ./test-build
   ```
   期待される出力: `142`

6. **クリーンアップ確認**:
   - `test-build.c` は自動削除されているはず
   - 失敗時のみ `.c` ファイルが残る

## 注意点

- WSL2 では `gcc` が PATH に通っているはずなので、`findGcc` は単に `"gcc"` を返すはず
- runtime/spinor.c と runtime/spinor.h は既に存在
- Windows 環境では MSYS2 の gcc が bash/PowerShell から正常に呼び出せない問題があった

## 残りの作業

動作確認のみ。実装は完了しています。
