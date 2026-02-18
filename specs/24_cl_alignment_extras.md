# Step 24: その他のCL互換機能 (Common Lisp Alignment Extras) 仕様書

## 概要
Common Lisp との親和性を高めるため、変数定義、制御構文、エラー処理に関する機能を追加する。
これらはカーネルレベルのプリミティブと、標準ライブラリのマクロの双方で実装される。

## 1. カーネル拡張 (Primitives)

### \`(error message)\`
- **説明:** 実行時エラーを意図的に発生させる。
- **引数:** \`message\` (文字列)。
- **動作:** 指定されたメッセージとともに評価を中断し、エラーを報告する。
- **例:** \`(error "Something went wrong")\`

### \`(bound? symbol)\`
- **説明:** シンボルが現在の環境で定義されているか（値が束縛されているか）を判定する。
- **引数:** \`symbol\` (シンボル)。
- **戻り値:** 定義済みなら \`#t\`、そうでなければ \`#f\`。
- **用途:** \`defvar\` の実装に必要。

## 2. 標準ライブラリ拡張 (Macros)

### \`(defvar name [initial-value [doc-string]])\`
- **説明:** グローバル変数を定義する。ただし、**既に定義されている場合は値を上書きしない**。
- **動作:**
  1. \`(bound? 'name)\` が真なら、何もしない（現在の値を維持）。
  2. 偽なら、\`initial-value\` を評価して \`name\` に代入する。
- **互換性:** Common Lisp の \`defvar\` 準拠。

### \`(defparameter name initial-value [doc-string])\`
- **説明:** グローバル変数を定義する。**常に値を上書きする**。
- **動作:** \`define\` とほぼ同じだが、意図を明示するために用意する。
- **互換性:** Common Lisp の \`defparameter\` 準拠。

### \`(let* bindings body...)\`
- **説明:** 順次束縛を行う \`let\`。前の変数を後の定義で参照できる。
- **構文:** \`(let* ((var1 val1) (var2 val2)) body...)\`
- **展開:** ネストした \`let\` に展開される。
  \`\`\`lisp
  (let ((var1 val1))
    (let ((var2 val2))
      body...))
  \`\`\`

### \`(dolist (var list-form [result-form]) body...)\`
- **説明:** リストの各要素に対して反復処理を行う。
- **引数:**
  - \`var\`: 反復ごとに現在の要素が束縛される変数。
  - \`list-form\`: リストを生成する式。
  - \`result-form\`: (オプション) ループ終了後に評価され、戻り値となる式。省略時は \`nil\`。
- **動作:** \`foldl\` や再帰関数を使って実装する。

### \`(dotimes (var count-form [result-form]) body...)\`
- **説明:** 指定回数だけ反復処理を行う。
- **引数:**
  - \`var\`: 0 から count-1 までカウントアップする変数。
  - \`count-form\`: 繰り返し回数（整数）。
- **動作:** 再帰関数を使って実装する。
