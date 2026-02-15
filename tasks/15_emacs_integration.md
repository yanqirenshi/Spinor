# タスク: ステップ15 - Emacs 開発環境 (spinor-mode.el) の構築

## 目標

Emacs ユーザーである開発者のために、Spinor 専用のメジャーモードを作成する。
SLY 相当とまではいかないが、`run-spinor` による REPL 起動と、バッファからのコード送信 (`eval-last-sexp`) を可能にする。

## 実装詳細指示

### 1. ディレクトリ作成

* プロジェクトルートに `editors/emacs/` ディレクトリを作成してください。

### 2. `editors/emacs/spinor-mode.el` の作成

以下の機能を持つ Emacs Lisp ファイルを作成してください。

* **`spinor-mode` (メジャーモード):**
    * `lisp-mode` を継承（これで基本的なインデントや括弧処理は自動で対応されます）。
    * 拡張子 `.spin` に関連付け。
    * **キーワードのハイライト:**
        * 定義系: `define`, `def`, `mac`, `fn`, `let`
        * 制御系: `if`, `cond`, `load`, `quote`
        * 定数: `#t`, `#f`, `nil`
    * コメント文字を `;` に設定。

* **`inferior-spinor-mode` (REPLモード):**
    * `comint-mode` を継承。
    * プロンプトの正規表現: `^spinor> `
    * `cabal run spinor` (またはバイナリ直接指定) でプロセスを起動する `run-spinor` コマンドを実装。
        * ※ `cabal` のビルドログが邪魔にならないよう、`cabal -v0 run spinor` とする等の工夫があると良い。

* **キーバインドと連携:**
    * `C-x C-e`: カーソル直前の S式 を REPL プロセスに送信して評価する機能。
    * `C-c C-k`: バッファ全体をロード (`load` コマンドを送信) する機能。

## コードのイメージ (参考)

```elisp
(defun spinor-eval-last-sexp ()
  (interactive)
  (let ((str (buffer-substring-no-properties ...)))
    (comint-send-string "*spinor*" (concat str "\n"))))

```

## 出力要件

* `editors/emacs/spinor-mode.el` の完全なコード。
* ユーザー (Emacs設定ファイル) への設定手順の解説。
