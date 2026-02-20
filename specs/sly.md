# SLY 統合仕様書

## 概要

Spinor は SLY (Sylvester the Cat's Common Lisp IDE) との統合をサポートしています。Swank プロトコルを実装し、Emacs から Spinor サーバーに接続して対話的な開発が可能です。

## 接続方法

```bash
# サーバー起動
cabal run spinor -- server

# Emacs から接続
M-x sly-connect RET localhost RET 4005
```

## 実装済み Contrib

| Contrib | 状態 | 説明 |
|---------|------|------|
| slynk/arglists | 完了 | 関数シグネチャの表示 (autodoc) |
| slynk/completion | 完了 | 補完機能 (flex-completions) |
| slynk/indentation | 完了 | インデント情報の提供 |
| slynk/fancy-inspector | 完了 | オブジェクトインスペクタ |
| slynk/mrepl | 完了 | マルチ REPL チャンネル |
| slynk-trace-dialog | 完了 | 関数トレース機能 |
| slynk-stickers | 完了 | スティッカー (値の記録) |
| slynk-profiler | 完了 | プロファイリング |
| slynk-package-fu | 完了 | パッケージ操作 |
| slynk-apropos | 完了 | シンボル検索 |
| slynk-disassemble | 完了 | 関数のディスアセンブル (AST 表示) |
| slynk-macrostep | TODO | マクロ展開 (Emacs 側の依存あり) |
| slynk-xref | TODO | クロスリファレンス (静的解析が必要) |

## RPC ハンドラ一覧

### 基本操作

- `swank:connection-info` - 接続情報の取得
- `swank:create-repl` - REPL の作成
- `swank:listener-eval` - コード評価
- `swank:interactive-eval` - 対話的評価
- `swank:eval-and-grab-output` - 評価と出力の取得

### 補完・ドキュメント

- `swank:completion:flex-completions` - フレックス補完
- `swank:autodoc` - 関数シグネチャの自動表示
- `swank:operator-arglist` - 演算子引数リスト

### インスペクタ

- `swank:init-inspector` - インスペクタ初期化
- `swank:eval-for-inspector` - インスペクタ用評価
- `swank:inspector-nth-part` - N番目の部品取得
- `swank:inspect-nth-part` - N番目の部品をインスペクト

### トレース・プロファイリング

- `swank:trace:dialog-toggle-trace` - トレースのトグル
- `swank:trace:report-specs` - トレース仕様のレポート
- `swank:profiler:toggle-timing` - タイミング計測のトグル
- `swank:profiler:report-latest-timings` - 最新タイミングのレポート
- `swank:profiler:clear-timing-tree` - タイミングツリーのクリア
- `swank:profiler:untime-all` - 全タイミングの解除

### スティッカー

- `swank:stickers:total-recordings` - 記録総数
- `swank:stickers:fetch` - スティッカーデータの取得
- `swank:stickers:forget` - スティッカーの削除

### パッケージ

- `swank:package-fu:list-all-package-names` - 全パッケージ名リスト
- `swank:package-fu:set-package` - パッケージの設定

### シンボル検索

- `swank:apropos:apropos-list-for-emacs` - シンボル検索

### ディスアセンブル

- `swank:disassemble-form` - フォームのディスアセンブル
- `swank:disassemble-symbol` - シンボルのディスアセンブル

### クロスリファレンス (スタブ)

- `swank:xref` - クロスリファレンス検索 (空リストを返す)

## コマンド正規化

SLY は `slynk:` プレフィックスを使用しますが、Spinor は内部的に `swank:` プレフィックスに正規化します。

| SLY コマンド | 内部コマンド |
|-------------|-------------|
| `slynk:autodoc` | `swank:autodoc` |
| `slynk-completion:flex-completions` | `swank:completion:flex-completions` |
| `slynk-mrepl:create-mrepl` | `swank:mrepl:create-mrepl` |
| `slynk-trace-dialog:dialog-toggle-trace` | `swank:trace:dialog-toggle-trace` |
| `slynk-stickers:fetch` | `swank:stickers:fetch` |
| `slynk-profiler:toggle-timing` | `swank:profiler:toggle-timing` |
| `slynk-package-fu:list-all-package-names` | `swank:package-fu:list-all-package-names` |
| `slynk-macrostep:macrostep-expand-1` | `swank:macrostep:macrostep-expand-1` |
| `slynk-apropos:apropos-list-for-emacs` | `swank:apropos:apropos-list-for-emacs` |
| `slynk-xref:xref` | `swank:xref:xref` |

## TODO

### slynk-macrostep

- **ファイル**: `tasks/todo_202602201030_macrostep.md`
- **課題**: Emacs 側で `sly-macrostep` パッケージのインストールが必要
- **サーバー側**: 実装済み (`macrostep-expand-1`, `macrostep-expand` など)

### slynk-xref (静的解析)

- **ファイル**: `tasks/todo_202602201100_xref_static_analysis.md`
- **課題**: 呼び出し関係の静的解析が必要
- **現状**: スタブ実装 (空リストを返す)
- **必要な作業**:
  - CallGraph データ構造の実装
  - AST 解析による呼び出し関係の抽出
  - ソース位置の追跡

## テスト

### ユニットテスト

```bash
cabal test
```

`Spinor.ServerSpec` に以下のテストが含まれています:
- コマンド正規化 (`normalizeCommand`)
- フォーム正規化 (`normalizeForm`)
- トレース仕様抽出 (`extractTraceSpec`)
- レスポンスビルダー (`mkOkResponse`, `mkAbortResponse`)
- TracedFunctions 状態管理

### E2E テスト

```bash
# サーバー起動後、Emacs で実行
M-x load-file RET test/e2e/sly-test.el
M-x spinor-run-e2e-tests
```

## 関連ファイル

- `src/Spinor/Server.hs` - メインの RPC ハンドラ実装
- `test/Spinor/ServerSpec.hs` - ユニットテスト
- `test/e2e/sly-test.el` - E2E テスト
- `tasks/todo_202602201030_macrostep.md` - Macrostep TODO
- `tasks/todo_202602201100_xref_static_analysis.md` - Xref TODO
