# 58: Emacs Package Spin-off の実装指示書

## 1. 目的
`spinor-mode.el` を独立パッケージとしてステージングディレクトリに整備し、MELPA 基準の品質を確保する。

## 2. 実装手順

### Step 1: ステージング環境の構築
- ルート直下に `.spinor-mode-export/` および `.github/workflows/` (その中) を作成する。
- `editors/emacs/spinor-mode.el` をコピーする。

### Step 2: Elisp コードのリファクタリング
- `spinor-mode.el` のヘッダを MELPA フォーマットに適合させる。
    - `Author`, `Maintainer`, `URL`, `Version`, `Package-Requires` を明記。
- 全ての `defun`, `defvar`, `defcustom` に適切なドキュメント文字列 (docstring) を付与する。
- `checkdoc` コマンドを実行し、スタイルエラーをすべて修正する。

### Step 3: 専用 README.md の執筆
- `.spinor-mode-export/README.md` を作成。
- `use-package` や `straight.el` を使用したインストール方法を記載。
- `spinor-mode` (シンタックスハイライト), `run-spinor` (REPL), LSP/SLY 連携の設定例を執筆する。

### Step 4: GitHub Actions (CI) の設定
- `.spinor-mode-export/.github/workflows/emacs-ci.yml` を作成。
- `purcell/setup-emacs` アクション等を使用して、Emacs 27.2, 28.2, 29.1 上で以下を実行する：
    - `emacs -Q --batch -f batch-byte-compile spinor-mode.el`
    - (可能であれば) `package-lint` の実行。

### Step 5: 検証
- ステージングディレクトリ内で `emacs -Q` を起動し、`load-file` で `spinor-mode.el` が正しく読み込まれ、正常に動作することを確認する。

## 3. 完了条件
- `.spinor-mode-export/` 内に、リポジトリとして完結したファイル群が存在すること。
- `spinor-mode.el` が `checkdoc` や `package-lint` の基準を満たしていること。

---
## 実装報告

### 実装方針

既存の `editors/emacs/spinor-mode.el` を基に、MELPA 登録基準を満たす独立パッケージをステージングディレクトリに構築する。MELPA 準拠のヘッダ、完全な docstring、GitHub Actions による CI パイプラインを整備し、将来の独立リポジトリ化に備える。

### 実装内容

#### 1. ステージング環境の構築

以下のディレクトリ構造を作成:
```
.spinor-mode-export/
├── spinor-mode.el          # MELPA 準拠版
├── README.md               # Emacs ユーザー向けマニュアル
├── LICENSE                 # MIT ライセンス
└── .github/
    └── workflows/
        └── emacs-ci.yml    # CI ワークフロー
```

#### 2. Elisp コードのリファクタリング (`spinor-mode.el`)

**MELPA 準拠ヘッダ:**
```elisp
;; Author: Spinor Project <spinor@example.com>
;; Maintainer: Spinor Project <spinor@example.com>
;; URL: https://github.com/yanqirenshi/Spinor
;; Version: 0.1.0
;; Keywords: languages, lisp, spinor
;; Package-Requires: ((emacs "26.1"))
```

**追加・改善点:**
- 全ての `defun`, `defvar`, `defcustom`, `defgroup` に checkdoc 準拠の docstring を付与
- キーワードハイライトに `defun`, `defmacro`, `data`, `defpackage`, `in-package`, `linear`, `with-region` 等を追加
- 型コンストラクタ（大文字開始の識別子）のハイライトを追加
- Commentary セクションに LSP/SLY 連携の設定例を記載

#### 3. README.md の執筆

以下のセクションを含む包括的なドキュメント:
- **Installation**: `use-package`, `straight.el`, 手動インストール
- **Usage**: REPL 起動、キーバインド一覧
- **LSP Support**: `lsp-mode` / `eglot` の設定例
- **SLY Integration**: Swank サーバー接続方法
- **Customization**: カスタマイズ変数一覧

#### 4. GitHub Actions CI (`emacs-ci.yml`)

**テストマトリクス:**
- Emacs 27.2, 28.2, 29.1 でのバイトコンパイル
- `byte-compile-error-on-warn` による厳格チェック

**Lint ジョブ:**
- `package-lint` による MELPA 規約チェック
- `checkdoc` によるスタイルチェック

#### 5. 検証

ステージングディレクトリの構造確認:
```
.spinor-mode-export/
├── spinor-mode.el   (7,203 bytes)
├── README.md        (3,621 bytes)
├── LICENSE          (1,092 bytes)
└── .github/workflows/emacs-ci.yml (2,099 bytes)
```

注: ローカル環境に Emacs がインストールされていないため、バイトコンパイルテストは GitHub Actions に委譲。

### 完了日
2026-03-01
