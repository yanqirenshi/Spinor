# Step 30: SLY Integration (Swank Protocol Basics) - 技術仕様

## 1. 概要

本仕様は、Spinor 処理系と Emacs Lisp 開発環境 SLY (SLIME) との連携を可能にするため、SLY が用いる通信プロトコル **Swank** の基本部分を実装するための技術仕様を定義する。

このステップでは、プロトコルのパケット構造 (Framing)、基本的な RPC (Remote Procedure Call) の応答、および接続時のハンドシェイクを対象とする。

## 2. Swank プロトコル仕様 (Phase 1)

### 2.1. Framing (パケット構造)

Swank プロトコルでは、TCP ストリーム上でメッセージの境界を明確にするため、すべてのメッセージ（ペイロード）の前にその長さをヘッダとして付与する。

-   **形式:** `YYYYYYPAYLOAD`
    -   **`YYYYYY` (ヘッダ):** ペイロードの**バイト長**を、ゼロパディングされた**6桁の16進数文字列**で表現したもの。ペイロードを UTF-8 でエンコードした後のバイト数を指定する。
    -   **`PAYLOAD` (ペイロード):** Lisp の S式 を UTF-8 で文字列化したもの。

-   **例:**
    -   ペイロードが `(+ 1 2)` の場合、UTF-8 バイト長は `7`。
    -   ヘッダは `000007` となる。
    -   送信されるパケット全体は `000007(+ 1 2)` となる。

### 2.2. RPC (Remote Procedure Call) モデル

通信は、クライアント (SLY) からの RPC リクエストと、サーバー (Spinor) からの RPC レスポンスによって行われる。

#### 2.2.1. RPC リクエスト

クライアントは、サーバーに処理を依頼するため、以下の形式の S式 をペイロードとして送信する。

-   **形式:** `(:emacs-rex (form) package thread-id request-id)`
    -   `ESym ":emacs-rex"`: RPC リクエストを示すキーワードシンボル。
    -   `(form)`: 実際にサーバーに評価または実行してほしい S式。例: `(swank:connection-info)`
    -   `package`: 対象のパッケージ名 (文字列)。Spinor には Common Lisp のパッケージシステムがないため、`"user"` や `nil` として扱われる。
    -   `thread-id`: リクエスト元のスレッドID。
    -   `request-id`: リクエストを一意に識別する整数。サーバーはこの ID をレスポンスに含める必要がある。

#### 2.2.2. RPC レスポンス

サーバーは、リクエストを処理した後、結果を以下の形式の S式 で返す。

-   **成功時の形式:** `(:return (:ok result-value) request-id)`
    -   `result-value`: `form` を処理した結果。S式で表現される。
    -   `request-id`: 対応するリクエストの ID。

-   **エラー時の形式 (簡易版):** `(:return (:abort "error-message") request-id)`
    -   `"error-message"`: エラー内容を記述した文字列。
    -   `request-id`: 対応するリクエストの ID。

### 2.3. Handshake (接続シーケンス)

SLY がサーバーに接続すると、最初に行うのがハンドシェイクである。

1.  **クライアントからのリクエスト:** SLY は、サーバーの情報を問い合わせるため、`swank:connection-info` フォームを含む RPC リクエストを送信する。
    ```lisp
    (:emacs-rex (swank:connection-info) "user" t 1)
    ```

2.  **サーバーからのレスポンス:** サーバーは、自身の情報をプロパティリスト (plist) 形式の S式 で返す。
    -   **必須フィールド:** `:pid`, `:style`, `:lisp-implementation`, `:version`
    -   **レスポンスのペイロード例:**
        ```lisp
        (:return
         (:ok (:pid nil
               :style ":spawn"
               :lisp-implementation (:type "Spinor" :version "0.1.0" :package (:name "user" :prompt "SPINOR>"))
               :version "2.27" ; SLY との互換性のため、Swank のバージョンを返す
               ))
         1)
        ```
    -   `pid` は取得が難しい場合 `nil` でも可。`:style` はスレッドモデルを示し、`:spawn` (スレッドを生成する) が一般的。
