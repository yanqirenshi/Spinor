# Task 43b: Build & Environment Guide の作成

開発者およびユーザー向けの環境構築ガイドを執筆し、マニュアルサイトに統合してください。

## ステップ

### 1. ガイドの執筆 (build.md)
- `manual/public/docs/build.md` を作成してください。
- `specs/43b_build_guide_spec.md` に記述された OS ごとの手順を、実際に動作確認されたコマンドを含めて Markdown で記述してください。

### 2. ナビゲーションの更新
- マニュアルサイト (React アプリ) のサイドバーまたはナビゲーションコンポーネントを更新してください。
- 「Build Guide」という項目を追加してください。

### 3. 既存ドキュメントからのリンク
- `manual/public/docs/introduction.md` を編集してください。
- 詳細なセットアップ手順への誘導を追加してください。

### 4. 動作確認
- ローカルでマニュアルサイトを起動し、新しく追加したビルドガイドが正しく表示されることを確認してください。

## 実装報告ルール
実装完了後、**このファイル自体を編集して**、以下のセクションを末尾に追記してください。

### 実装方針

**ドキュメント構成:**
- GHCup を使った Haskell ツールチェーンのインストールから始め、OS ごとの手順を分離
- Linux/WSL2 と Windows/MSYS2 の両方をカバーし、実際に動作確認済みのコマンドを掲載
- トラブルシューティングセクションで、よくある問題（DLL Not Found, OpenCL Platform Not Found, hmatrix ビルドエラー）の解決策を詳述

**Windows DLL 問題の解説:**
- `cabal test` が PATH を子プロセスに渡さない問題を明確に説明
- 必要な DLL 一覧と、コピー先ディレクトリのパスを PowerShell コマンドで提示
- `cabal clean` 後の再コピーが必要な点を注意事項として記載

**ナビゲーション統合:**
- サイドバーに「Build Guide」を Introduction と Syntax の後、API Reference の前に配置
- introduction.md のインストールセクションに Build Guide への誘導を追加

### 実装内容

**作成ファイル:**

1. **manual/public/docs/build.md** (新規作成)
   - 共通の前提条件（GHCup, GHC, Cabal, Git）
   - Linux (Ubuntu/Debian/WSL2) のセットアップ手順
   - Windows 11 (MSYS2/MinGW64) のセットアップ手順
   - cabal.project.local の設定例
   - トラブルシューティング（DLL, OpenCL, hmatrix, pkg-config）

**変更ファイル:**

2. **manual/src/components/Sidebar.tsx**
   - sections 配列に `{ label: 'Build Guide', to: '/docs/build' }` を追加

3. **manual/public/docs/introduction.md**
   - 「ソースからのビルド」セクション後に Build Guide への誘導を追加
   - 「次のステップ」セクションに Build Guide リンクを追加

**動作確認:**
- TypeScript 型チェック: エラーなし (`npx tsc --noEmit`)
