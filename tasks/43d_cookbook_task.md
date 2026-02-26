# 43d: Spinor Cookbook の実装指示書

## 1. 目的
実践的なコード例をまとめた Cookbook を作成し、マニュアルの有用性を高める。

## 2. 実装手順

### Step 1: Cookbook ページの作成
- `manual/public/docs/cookbook.md` を作成し、以下のレシピを執筆する。
    - **安全なファイル操作:** `unwind-protect` を使用した例。
    - **JSON 操作:** API レスポンスを模した文字列をパースし、要素を抽出する例。
    - **並行処理:** `spawn` と `take-mvar`, `put-mvar` を用いた同期パターン。
    - **行列計算:** `matrix` プリミティブと `m*`, `transpose` の活用例。
- 各コード例には、何を行っているかを示すコメントを Spinor 形式 (`;`) で記述すること。

### Step 2: サイドバーへの登録
- `manual/src/components/Sidebar.tsx` を編集し、`sections` 配列に Cookbook へのリンクを追加する。
- 順序は `Introduction` の直後、`Syntax` の前とする。

### Step 3: シンタックスハイライトの確認
- `manual/` ディレクトリで `npm run dev` (または適切なコマンド) を実行し、ブラウザで Cookbook ページを確認する。
- コードブロック内の Lisp 構文が正しくハイライトされていることを確認。

### Step 4: ビルド確認
- `cabal run spinor -- docgen` を実行し、マニュアル全体が整合性を持って出力されることを確認。

## 3. 完了条件
- `cookbook.md` が作成され、マニュアルのナビゲーションからアクセス可能であること。
- 記述されたコード例が Spinor の最新のプリミティブ仕様に基づいていること。

---
## 実装報告

### 実装方針

ユーザーが「やりたいこと」から逆引きできる形式で、4つの実践的なレシピを作成。各レシピには基本パターン、応用例、実践例の3段階を設け、段階的に理解を深められる構成とした。コードには日本語コメントを付与し、そのままコピー＆ペーストで試せるようにした。

### 実装内容

#### 作成ファイル

**`manual/public/docs/cookbook.md`**
- **安全なファイル処理 (Resource Protection)**
  - `unwind-protect` を使った基本パターン
  - `handler-case` と組み合わせたエラー時デフォルト値
  - 設定ファイルの読み書き実践例
- **JSON データの抽出と変換 (Web API Mockup)**
  - `json-parse` と `assoc` による基本抽出
  - ネストした JSON のアクセスヘルパー
  - API レスポンスのバリデーション実践例
- **並行ワーカーパターン (Concurrency with MVar)**
  - `spawn` + `MVar` の基本同期
  - 複数ワーカーの並列実行
  - プロデューサー・コンシューマーパターン
- **科学技術計算と GPU 演算 (Matrix & OpenCL)**
  - `matrix` による行列生成と基本演算
  - 転置と行列加算
  - OpenCL カーネルによる GPU 並列計算
  - 逆行列計算
- **Tips & ベストプラクティス**
  - エラーハンドリングの組み合わせ
  - 関数型スタイルでのデータ変換
  - デバッグのコツ

#### 変更ファイル

**`manual/src/components/Sidebar.tsx`**
- `Introduction` の直後に `Cookbook (逆引きレシピ)` へのリンクを追加

#### docgen 結果

```
Generated 70 reference files.
Documentation generated successfully.
```

### 完了日
2026-02-26
