# Step 30: SLY Integration (Swank Protocol Basics) - 実装指示書

## 概要

このタスクでは、Step 29 で作成した TCP サーバーを、SLY/SLIME が通信できる Swank プロトコルの基本形に対応させます。具体的には、パケットのフレーミング、RPCリクエストの解釈と応答、接続時のハンドシェイクを実装します。

仕様書 `specs/30_swank.md` に基づき、以下の手順で実装を進めてください。

## Steps (実装手順)

### 1. プロトコル層の実装 (`src/Spinor/Server.hs`)

Step 29 の行ベースのプロトコルを、Swank のヘッダ付きプロトコルに置き換えます。`Handle` よりも `ByteString` を直接扱う方が確実なため、`Network.Socket.ByteString` を使用します。

1.  **インポートの追加:** `Network.Socket.ByteString`, `Data.ByteString`, `Numeric` (16進数変換用), `Text.Printf` (ゼロパディング用) をインポートします。

2.  **`recvPacket` 関数の実装:**
    -   ソケットから正確に6バイト（ヘッダ）を `recv` します。
    -   ヘッダを16進数文字列として解釈し、ペイロード長を計算します。
    -   計算した長さの分だけ、ペイロード本体を `recv` します。
    -   ペイロード (`ByteString`) を UTF-8 でデコードして `Text` として返します。

    ```haskell
    -- ヘッダを読み、ペイロード長を返す
    recvHeader :: Socket -> IO Int
    recvHeader sock = do
        headerBytes <- BS.recv sock 6
        -- 16進数文字列をパース
        case readHex (decodeUtf8 headerBytes) of
            [(len, "")] -> pure len
            _           -> error "Invalid header"

    -- ペイロードを受信する
    recvPayload :: Socket -> Int -> IO T.Text
    recvPayload sock len = decodeUtf8 <$> BS.recv sock len
    ```

3.  **`sendPacket` 関数の実装:**
    -   送信したい `Text` をペイロードとして、UTF-8 の `ByteString` にエンコードします。
    -   バイト長を計算し、`printf "%06x"` で6桁の16進数ヘッダ文字列を作成します。
    -   ヘッダとペイロードを結合した `ByteString` を `sendAll` で送信します。

    ```haskell
    sendPacket :: Socket -> T.Text -> IO ()
    sendPacket sock payload = do
        let payloadBytes = encodeUtf8 payload
            len = BS.length payloadBytes
            header = encodeUtf8 $ T.pack $ printf "%06x" len
        BS.sendAll sock (header <> payloadBytes)
    ```

4.  **`clientLoop` の修正:**
    -   `Handle` を使うのをやめ、`Socket` を直接引数に取ります。
    -   ループ内で `hGetLine` の代わりに `recvHeader` と `recvPayload` を呼び出してリクエストを取得します。
    -   レスポンスは `hPutStrLn` の代わりに `sendPacket` で送信します。

### 2. ディスパッチャの実装 (`src/Spinor/Server.hs`)

受信した Swank リクエストを解釈し、適切な処理に振り分けます。

1.  **`clientLoop` 内の処理を変更:**
    -   受信したペイロードを `readExpr` でパースします。
    -   パース結果が `(EList [ESym ":emacs-rex", form, _, _, EInt reqId])` という構造にマッチするか確認します。
    -   マッチした場合、`form` と `reqId` を取り出して `handleSwankRequest` のような新しい関数に渡します。
    -   マッチしない場合は、エラーレスポンスを返します。
2.  **`handleSwankRequest` 関数の実装:**
    -   `form` の内容をさらにパターンマッチします。
        -   `form` が `(EList [ESym "swank:connection-info"])` の場合:
            - ハンドシェイク応答用の S式を構築します。（仕様書参照）
            - `(:return (:ok <response-plist>) <req-id>)` という最終的なレスポンス S式 を作成し、`show` で文字列化して `sendPacket` で送ります。
        -   `form` が `(EList [ESym "swank:eval-string", EStr code, ...])` の場合:
            - `code` (Text) を `readExpr` でパースし、`eval` で評価します。
            - 評価結果 (`Val`) を `valToExpr` で `Expr` に変換します。
            - `(:return (:ok <result-expr>) <req-id>)` を構築して送信します。
        -   その他の `form` の場合: 未実装を示すエラーを返します。

### 3. 動作確認

1.  **サーバーを起動します。**
    ```sh
    cabal run spinor -- server --port 4005
    ```
2.  **`netcat` で手動テスト:**
    -   別のターミナルで `nc -c localhost 4005` を実行します (`-c` は CRLF を送信するためのオプション、環境によっては不要)。
    -   以下の文字列を（改行なしで）貼り付けて送信します。ペイロードの長さは `47` なので、ヘッダは `00002f` です。
        ```
        00002f(:emacs-rex (swank:connection-info) "user" t 1)
        ```
    -   ヘッダ付きで `(:return (:ok ...))` という形式の応答が返ってくることを確認します。
3.  **(目標) SLY で接続テスト:**
    -   Emacs を起動し、`M-x sly-connect` を実行します。
    -   Host: `localhost`, Port: `4005` と入力します。
    -   接続が成功し、REPL が表示されたら、簡単な S式 `(+ 1 2)` を評価してみてください。

---

### 実装報告

**実装完了後、この Markdown ファイルを直接編集し、以下の2つのセクションを追記して実装内容を報告してください。**

#### 実装方針

1. **Handle ベースの実装を採用**: タスク指示書では `Network.Socket.ByteString` を推奨していたが、Windows 環境での `network` パッケージの再ビルド問題を回避するため、`System.IO` の `Handle` を使用して ByteString を読み書きする方式を採用した。`BS.hGet` と `BS.hPut` を使用することで、同等の機能を実現。

2. **Expr を Lisp 形式の文字列に変換する関数 (`exprToText`) を新規実装**: Swank プロトコルでは S式 形式での通信が必要なため、`Expr` 型を Lisp の構文に従った文字列に変換する関数を追加。

3. **RPC ディスパッチャのパターンマッチによる拡張性**: `handleSwankRequest` 関数で Swank コマンドをパターンマッチし、将来的なコマンド追加を容易にする設計。

4. **SLY 互換性のための追加コマンド対応**: `swank:connection-info` と `swank:listener-eval` に加え、SLY が送信する `swank:swank-require`, `swank:create-repl`, `swank:autodoc`, `swank:operator-arglist` にも最低限の応答を実装。

#### 実装内容

**変更ファイル:** `src/Spinor/Server.hs`

**追加した主要な関数:**

| 関数名 | 役割 |
|--------|------|
| `recvHeader` | 6バイトのヘッダを読み取り、ペイロード長を返す |
| `recvPayload` | 指定された長さのペイロードを受信し、UTF-8 デコードして Text を返す |
| `sendPacket` | Text をヘッダ付きでパケット送信する |
| `exprToText` | Expr を Lisp 形式の文字列に変換 |
| `escapeString` | 文字列内の特殊文字をエスケープ |
| `handleSwankRequest` | Swank RPC リクエストのディスパッチャ |
| `evalAndRespond` | S式を評価してレスポンスを送信 |
| `mkOkResponse` | 成功レスポンスを構築 |
| `mkAbortResponse` | エラーレスポンスを構築 |
| `mkConnectionInfoResponse` | connection-info のレスポンスを構築 |
| `dispatchRPC` | `:emacs-rex` 形式のパターンマッチ |

**動作確認結果:**

```bash
# connection-info テスト
$ printf '00002f(:emacs-rex (swank:connection-info) "user" t 1)' | nc localhost 4005
0000d6(:return (:ok (:pid 0 :style :spawn :encoding (:coding-systems ("utf-8-unix")) :lisp-implementation (:type "Spinor" :version "0.1.0" :program "spinor" :package (:name "user" :prompt "SPINOR>")) :version "2.27")) 1)

# listener-eval テスト
$ printf '000037(:emacs-rex (swank:listener-eval "(+ 1 2)") "user" t 2)' | nc localhost 4005
000015(:return (:ok "3") 2)
```

**ビルドに関する注意:**
- Windows 環境では `network` パッケージのビルドに msys2 シェルの使用が必要な場合がある
- ビルドコマンド: `C:/ghcup/msys64/usr/bin/bash.exe -lc "cd /c/Users/yanqi/prj/Spinor && cabal build"`
