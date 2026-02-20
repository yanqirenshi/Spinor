# sly-macrostep 対応

## 概要

SLY macrostep 機能の完全な統合。サーバー側の実装は完了しているが、動作確認と調整が必要。

## 現状

- サーバー側の RPC ハンドラは実装済み (`Server.hs`)
  - `macrostep-expand-1` - 1段階展開
  - `macrostep-expand` - 完全展開
  - `compiler-macroexpand-1`
  - `compiler-macroexpand`
- `SLYNK/MACROSTEP` は modules リストに追加済み
- ユニットテスト・E2E テスト作成済み

## Emacs 側の要件

```elisp
;; MELPA からインストール
M-x package-install RET sly-macrostep RET

;; 設定に追加
(add-to-list 'sly-contribs 'sly-macrostep)
```

## 残作業

### 1. 動作確認

- [ ] Emacs で `sly-macrostep` をインストール
- [ ] サーバーに接続して macrostep が動作することを確認
- [ ] Spinor のマクロ (`when`, `unless` など) で展開をテスト

### 2. 潜在的な課題

- sly-macrostep が期待するレスポンス形式の検証
- エラーハンドリングの改善
- 複雑なマクロ（ネストしたマクロ）の展開テスト

### 3. ドキュメント

- README または docs/ に macrostep の使い方を追記

## 関連ファイル

- `src/Spinor/Server.hs` - RPC ハンドラ (実装済み)
  - `macrostepExpand1`, `macrostepExpandFull`, `macroExpandOnce`
  - `exprToLispText` - 展開結果の Lisp 形式出力
- `test/Spinor/ServerSpec.hs` - ユニットテスト
- `test/e2e/sly-test.el` - E2E テスト

## 優先度

低（基本的な SLY 統合は完了しており、本機能は発展的な機能）
