# Stickers 値記録機能の実装

## 概要

SLY stickers の完全な実装。現在は RPC ハンドラのスタブのみで、実際の値記録機能は未実装。

## 現状

- SLY からの RPC リクエストには応答できる
- `fetch` → 空リスト
- `total-recordings` → 0
- 実際の評価値の記録は行われない

## 必要な作業

### 1. 記録ストレージの実装

```haskell
-- Server.hs または新規モジュール
data StickerRecording = StickerRecording
    { recordingId :: Integer
    , stickerId :: Integer
    , recordedValue :: Val
    , timestamp :: UTCTime
    }

type StickerState = IORef [StickerRecording]
```

### 2. 評価器 (Eval.hs) の拡張

スティッカー付き式を評価時に記録する仕組み:

```haskell
-- 案1: 特殊形式として実装
-- (sticker id expr) → expr を評価し、結果を id と共に記録

-- 案2: 環境にスティッカー状態を追加
-- EvalM モナドに記録機能を組み込む
```

### 3. compile-for-stickers の実装

SLY から送られる instrumented コードを解析し、スティッカーIDを抽出:

```haskell
-- compile-for-stickers の引数:
-- new-stickers: 新しいスティッカーIDリスト
-- dead-stickers: 削除するスティッカーIDリスト
-- instrumented-string: スティッカー付きコード
-- original-string: 元のコード
-- buffer, position, filename: ソース位置情報
-- policy: コンパイルポリシー
```

### 4. fetch の実装

記録された値を SLY に返す:

```haskell
-- 戻り値の形式:
-- ((sticker-id description recordings-count armed-p) ...)
```

### 5. replay 機能の実装

- `search-for-recording` - 記録のナビゲーション
- `find-recording-or-lose` - 特定の記録の取得
- `inspect-sticker-recording` - 記録の検査

## 参考資料

- `slynk-stickers.lisp`: https://github.com/joaotavora/sly/blob/master/contrib/slynk-stickers.lisp
- `sly-stickers.el`: https://github.com/joaotavora/sly/blob/master/contrib/sly-stickers.el

## 優先度

低〜中（基本的な SLY 統合は完了しており、本機能は発展的な機能）

## 関連ファイル

- `src/Spinor/Server.hs` - RPC ハンドラ（スタブ実装済み）
- `src/Spinor/Eval.hs` - 評価器（拡張が必要）
