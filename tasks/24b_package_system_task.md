# 24b: パッケージ操作系マクロの実装指示書

## 1. 目的
`defpackage`, `in-package`, `use-package` を実装し、動的な名前空間の管理を可能にする。また、マニュアルとドキュメントメタデータを更新する。

## 2. 実装手順

### Step 1: 環境データ構造の改修 (`Val.hs` / `Eval.hs`)
- `type Env` を `Map Text Val` から、パッケージ情報を保持する構造体 `data Context` 等に変更することを検討する。
- `Eval` モナドの `StateT Env` を、新しい `Context` を管理するように更新する。
- **注意:** 既存の `Map.lookup` や `Map.insert` を使用している箇所を、パッケージを考慮したヘルパー関数（例: `lookupSymbol`, `defineSymbol`）に置き換える。

### Step 2: コアロジックの実装
- `defpackage`: 新しいパッケージエントリを作成するロジックを `Eval.hs` に追加。
- `in-package`: `currentPackage` ポインタを更新するロジックを追加。
- `use-package`: `usedPackages` リストを更新するロジックを追加。

### Step 3: プリミティブの登録と CLHS ドキュメント
- `src/Spinor/Primitive.hs` に上記 3 つの機能を登録する。
- `src/Spinor/Docs.hs` (またはドキュメント定義ファイル) に、これらの関数の CLHS 形式のドキュメント（Arguments, Description, Examples）を追記する。

### Step 4: マニュアルの更新
- `manual/public/docs/syntax/packages.md` を編集する。
    - 「将来の拡張」セクションを削除または更新し、実装されたパッケージ操作について記述する。
    - `defpackage`, `in-package` の具体例を追加。
    - REPL での作業フロー（パッケージ切り替え）の解説を追加。

### Step 5: 検証
- `test/Spinor/EvalSpec.hs` にパッケージテストを追加。
    - 別パッケージの未公開シンボルが見えないこと。
    - `use-package` 後に公開シンボルが見えるようになること。
- `cabal run spinor -- docgen` を実行し、マニュアルサイトが正しくビルドされることを確認。

## 3. 完了条件
- 名前空間の隔離と `use-package` による結合が意図通り動作すること。
- マニュアルの `packages.md` が最新の仕様を反映していること。

---
## 実装報告

### 実装方針

**CDD (Compiler-Driven Development)** アプローチを採用し、以下の順序で実装を進めた：

1. **データ型の拡張**: まず `Val.hs` に `Package` と `Context` 型を定義
2. **Eval モナドの改修**: `StateT Env` から `StateT EvalState` に変更し、`EvalState` が `esLocal` (ローカル環境) と `esContext` (パッケージコンテキスト) を保持
3. **コンパイルエラーの修正**: `cabal build` で発生したエラーを順次修正
4. **テスト追加と検証**: `EvalSpec.hs` にテストを追加し、全テストパスを確認
5. **ドキュメント整備**: `Docs.hs` と `packages.md` を更新

### 実装内容

#### 変更ファイル

**`src/Spinor/Val.hs`**
- `Package` データ型を追加: `pkgName`, `pkgBindings`, `pkgExports`, `pkgUsedPackages` を保持
- `Context` データ型を追加: `ctxCurrentPackage`, `ctxPackages` を保持
- `initialContext` を追加: `spinor` (コアパッケージ) と `user` (デフォルト) を初期化

**`src/Spinor/Eval.hs`**
- `Eval` モナドを `StateT Env` から `StateT EvalState` に変更
- `EvalState` 型を追加: ローカル環境とパッケージコンテキストを分離
- シンボル解決ヘルパーを追加:
  - `lookupSymbol`: レキシカル → カレントパッケージ → use されたパッケージ → spinor の順で検索
  - `defineSymbol`: カレントパッケージにシンボルを定義
  - `getLocalEnv`, `putLocalEnv`, `getContext`, `putContext`: 状態操作
- 特殊形式を追加:
  - `defpackage`: パッケージ定義 (`:use`, `:export` オプション対応)
  - `in-package`: カレントパッケージ切り替え
  - `use-package`: パッケージのインポート
  - `current-package`: カレントパッケージ名取得
  - `export`: シンボルのエクスポート

**`src/Spinor/Expander.hs`**
- マクロ展開時のシンボル検索を `lookupSymbol` に変更
- `liftIO` インポートを追加

**`src/Spinor/Server.hs`**
- `macroExpandOnce` のシンボル検索を `lookupSymbol` に変更

**`src/Spinor/Lsp/Docs.hs`**
- 5 つのパッケージ操作の CLHS ドキュメントを追加

**`test/Spinor/EvalSpec.hs`**
- 10 件のパッケージシステムテストを追加

**`manual/public/docs/syntax/packages.md`**
- パッケージシステムの詳細なドキュメントを追加
- REPL での使用例を追加
- 「将来の拡張」セクションを実装済み機能の説明に更新

#### シンボル解決の順序

1. レキシカルスコープ (`let`, `fn` 引数)
2. カレントパッケージの `pkgBindings`
3. `pkgUsedPackages` 内のパッケージの `pkgExports` に含まれるシンボル
4. `spinor` パッケージ (プリミティブ)

#### テスト結果

```
197 examples, 0 failures
```

すべてのテスト（既存 187 件 + 新規 10 件）がパス。

#### docgen 結果

```
Generated 67 reference files.
Documentation generated successfully.
```

### 完了日
2026-02-26
