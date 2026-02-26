# 24c: Error Handling & Condition System 仕様書

## 1. 概要
Spinor 言語において、実行時エラーを捕捉し、適切に復帰またはクリーンアップを行うための機構を導入する。これにより、異常系処理の記述を容易にし、リソースリーク（ファイルハンドルの開放漏れ等）を防ぐ。

## 2. アーキテクチャ
Spinor の評価モナド `Eval` は `MonadError SpinorError` インスタンスであるため、Haskell の `catchError` プリミティブを利用して言語レベルのエラーハンドリングを実現する。

### 捕捉対象
- `(error "msg")` プリミティブによる明示的なエラー
- ゼロ除算、未定義のシンボル、型エラー等のランタイムエラー

## 3. 制御構文 (特殊形式)

### `(ignore-errors &body body)`
- **機能:** `body` の評価中にエラーが発生した場合、エラーを無視して `nil` を返す。成功時は最後の結果を返す。
- **挙動:** `catchError` でエラーをトラップし、`VNil` を返却する。

### `(handler-case expression (error (var) handler-body ...))`
- **機能:** `expression` を評価し、エラーが発生した場合は指定されたハンドラを実行する。
- **引数:**
    - `error`: 現時点ではすべてのエラーを捕捉するキーワード（将来的に型による分岐を予定）。
    - `var`: エラーメッセージ（文字列）が束縛される変数名。
- **挙動:** `expression` を評価し、エラー時には `var` にエラー内容を束縛した環境で `handler-body` を評価する。

### `(unwind-protect protected-form cleanup-form ...)`
- **機能:** `protected-form` を評価し、その成否に関わらず必ず `cleanup-form` を実行する。
- **重要性:** ファイル入出力やロックの解除など、リソース保護に必須。
- **挙動:** `protected-form` の評価結果（またはエラー）を保持したまま `cleanup-form` を実行し、最後に `protected-form` の結果を返す（またはエラーを再送出する）。

## 4. ドキュメント戦略
- `manual/public/docs/syntax/conditions.md` を新設。
- Common Lisp の Condition System との対応関係（および簡略化されている点）を明記。
- ユーザーに `unwind-protect` の重要性を啓蒙する。
- サイドバー (`Sidebar.tsx`) の `Control Flow` の直後に `Conditions & Errors` を追加。
