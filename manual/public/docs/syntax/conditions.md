# Conditions & Errors

Spinor におけるエラーハンドリングと例外処理について解説します。

## 概要

Spinor は Common Lisp スタイルのエラーハンドリング機構を提供します。これにより、実行時エラーを捕捉し、適切に復帰またはクリーンアップを行うことができます。

## エラーの送出

### error

明示的にエラーを送出します。

```lisp
(error "ファイルが見つかりません")
```

エラーメッセージは文字列で指定します。送出されたエラーは、適切なハンドラで捕捉しない限りプログラムを終了させます。

## エラーの捕捉

### ignore-errors

式の評価中に発生したエラーを無視し、`nil` を返します。

```lisp
;; エラーを無視
(ignore-errors (error "fail"))  ; => nil

;; 正常時は値を返す
(ignore-errors (+ 1 2))  ; => 3

;; 複数の式を評価
(ignore-errors
  (print "start")
  (/ 10 0)        ; ゼロ除算エラー
  (print "end"))  ; => nil ("start" のみ出力)
```

**用途:**
- エラーが発生しても処理を続行したい場合
- エラーの詳細を気にしない場合

### handler-case

式を評価し、エラーが発生した場合は指定されたハンドラを実行します。

```lisp
(handler-case
  expression
  (error (variable) handler-body...))
```

- `expression` - 評価する式
- `error` - エラーハンドラのキーワード
- `variable` - エラーメッセージが束縛される変数
- `handler-body` - エラー時に評価される式

```lisp
;; エラーメッセージを取得
(handler-case
  (error "something went wrong")
  (error (msg)
    (string-append "Caught: " msg)))
; => "Caught: something went wrong"

;; デフォルト値を返す
(handler-case
  (/ 10 0)
  (error (_) 0))  ; => 0

;; 正常時はそのまま値を返す
(handler-case
  (+ 1 2)
  (error (e) "error"))  ; => 3
```

**用途:**
- エラーの内容に応じて処理を分岐したい場合
- エラーをログに記録したい場合
- デフォルト値を返したい場合

## リソース保護

### unwind-protect

保護された式を評価し、その成否に関わらずクリーンアップ式を必ず実行します。

```lisp
(unwind-protect
  protected-form
  cleanup-form...)
```

- `protected-form` - 保護される式 (1つ)
- `cleanup-form` - 必ず実行されるクリーンアップ式 (0個以上)

```lisp
;; 正常終了時もクリーンアップが実行される
(unwind-protect
  (+ 1 2)
  (print "cleanup done"))
; 出力: cleanup done
; => 3

;; エラー時もクリーンアップが実行される
(ignore-errors
  (unwind-protect
    (error "fail")
    (print "cleanup done")))
; 出力: cleanup done
; => nil
```

**重要性:**

`unwind-protect` は以下の場面で必須です：

1. **ファイル操作** - 開いたファイルを必ず閉じる
2. **ロック管理** - 取得したロックを必ず解放する
3. **一時リソース** - 確保したリソースを必ず解放する

```lisp
;; ファイル操作の典型パターン
(unwind-protect
  (begin
    (def data (read-file "input.txt"))
    (process data))
  (print "処理完了"))
```

## エラー型 (将来の拡張)

現在のバージョンでは、すべてのエラーは `error` キーワードで捕捉されます。将来的には、エラー型による分岐をサポートする予定です：

```lisp
;; 将来の構文 (未実装)
(handler-case
  (risky-operation)
  (file-error (e) (handle-file-error e))
  (network-error (e) (handle-network-error e))
  (error (e) (handle-generic-error e)))
```

## Common Lisp との比較

| Spinor | Common Lisp | 説明 |
|:-------|:------------|:-----|
| `ignore-errors` | `ignore-errors` | 同じ動作 |
| `handler-case` | `handler-case` | 簡易版 (型分岐なし) |
| `unwind-protect` | `unwind-protect` | 同じ動作 |
| - | `handler-bind` | 未サポート |
| - | `restart-case` | 未サポート |

Spinor のエラーハンドリングは Common Lisp の簡易版です。`handler-bind` や `restart-case` などの高度な機能はサポートされていませんが、基本的なエラー捕捉とリソース保護は十分に機能します。

## ベストプラクティス

1. **リソースは必ず `unwind-protect` で保護する**
   ```lisp
   (unwind-protect
     (begin
       (acquire-resource)
       (use-resource))
     (release-resource))
   ```

2. **エラーメッセージは具体的に**
   ```lisp
   (error (string-append "ファイル " filename " が見つかりません"))
   ```

3. **エラーハンドラは適切な粒度で配置**
   - 小さすぎる: 毎回の操作を handler-case で囲む → 冗長
   - 大きすぎる: プログラム全体を handler-case で囲む → エラー位置が不明

## Symbols

| Type | Name | Description |
|:-----|:-----|:------------|
| Special Form | [error](ref/error) | エラーを送出 |
| Special Form | [ignore-errors](ref/ignore-errors) | エラーを無視して nil を返す |
| Special Form | [handler-case](ref/handler-case) | エラーを捕捉してハンドラを実行 |
| Special Form | [unwind-protect](ref/unwind-protect) | クリーンアップを保証 |
