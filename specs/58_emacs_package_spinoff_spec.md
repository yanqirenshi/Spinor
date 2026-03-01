# 58: Emacs Package Spin-off Specification

## 1. 概要
現在の Spinor プロジェクトに含まれる `spinor-mode.el` を、独立したリポジトリとして抽出可能な「ステージング状態」に整備する。MELPA (Emacs Lisp Package Archive) の登録基準を満たすメタデータの付与と、パッケージ単体での品質保証 (CI) 体制を構築する。

## 2. アーキテクチャ (Standalone Staging)
プロジェクトルート直下の `.spinor-mode-export/` ディレクトリをステージング領域とし、以下の構成を維持する。

```
.spinor-mode-export/
├── spinor-mode.el      # 本体 (リファクタリング済み)
├── README.md           # Emacs ユーザー向け専用マニュアル
├── LICENSE             # ライセンスファイル (MIT または GPLv3)
└── .github/
    └── workflows/
        └── emacs-ci.yml # Emacs Lisp 向けの自動テスト (Lint/Compile)
```

## 3. パッケージメタデータ (MELPA 準拠)
`spinor-mode.el` のヘッダ部分を以下の要件に従って修正する。
- **Package-Requires:** Emacs の最小バージョン (26.1以上推奨) および依存パッケージ。
- **URL:** 本家のリポジトリまたは将来の専用リポジトリの URL。
- **Keywords:** 適切な検索タグ (`languages`, `lisp`, `spinor`)。
- **Documentation:** `checkdoc` が要求する形式の関数・変数ドキュメント。

## 4. 外部連携のサポート
独立パッケージ版でも、以下の Spinor 本体機能との連携方法を `README.md` で案内する。
- **SLY:** `M-x sly-connect` による接続方法。
- **LSP:** `lsp-mode` または `eglot` における `spinor lsp` コマンドの登録方法。

## 5. CI ワークフロー
GitHub Actions を用い、以下の検証を自動化する。
- **Byte Compilation:** コンパイル警告がないことの確認。
- **Linter:** `package-lint` による MELPA 規約のチェック、および `checkdoc` によるスタイルチェック。
