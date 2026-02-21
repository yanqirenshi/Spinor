# Task 42: GitHub Actions Integration

## 1. Objective

`specs/42_github_actions.md` の仕様に基づき、マニュアルサイト (`manual/`) のビルドと GitHub Pages へのデプロイを自動化する GitHub Actions ワークフローを実装する。

## 2. Implementation Steps

### Step 1: Create GitHub Actions Workflow File

以下の内容で `.github/workflows/deploy-docs.yml` を新規作成してください。

```yaml
name: Deploy Manual to GitHub Pages

on:
  push:
    branches:
      - main

jobs:
  build-and-deploy:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout repository
        uses: actions/checkout@v4

      - name: Setup Node.js
        uses: actions/setup-node@v4
        with:
          node-version: 20

      - name: Install dependencies and build site
        run: |
          cd manual
          npm install
          npm run build

      - name: Clear old docs directory
        run: rm -rf docs/*

      - name: Copy build output to docs directory
        run: |
          cp -r manual/dist/* docs/
          touch docs/.nojekyll

      - name: Commit and push changes
        uses: stefanzweifel/git-auto-commit-action@v5
        with:
          commit_message: "docs: Auto-deploy manual site from build"
          file_pattern: docs/* docs/.nojekyll
```

### Step 2: Update Vite Configuration

`manual/vite.config.ts` を開き、GitHub Pages でアセットが正しく読み込まれるように `base` プロパティを追加してください。

```typescript
import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [react()],
  // Add the base path for GitHub Pages
  base: '/Spinor/',
})
```

### Step 3: Clean Up Existing `docs/` Directory

現在の `docs/` ディレクトリには、手動で作成された古いドキュメントファイルが含まれています。これらは CI によって自動生成されるファイル群に置き換えられるため、不要になります。

以下のコマンドを実行して、古いファイルを Git から削除し、コミットしてください。

```sh
git rm -r docs/*
git commit -m "chore: Remove legacy docs files before GH Actions setup"
```
*Note: この操作は次の `push` で CI が実行される前に行っておくことが望ましいです。CI によって新しい `docs/` の内容がコミットされます。*

### Step 4: Verification (After Push)

上記の手順が完了したら、変更を `main` ブランチにプッシュしてください。
プッシュ後、GitHub の "Actions" タブで `Deploy Manual to GitHub Pages` ワークフローが実行され、成功することを確認してください。
ワークフロー完了後、GitHub Pages の URL (`https://<your-username>.github.io/Spinor/`) にアクセスし、マニュアルサイトが正しく表示されることを確認します。

## 3. Implementation Report

このタスクが完了したら、この Markdown ファイルの末尾に以下のセクションを追加し、実装内容を報告してください。

### 実装方針

- （ここに実装の概要やアーキテクチャ上の判断を記述）

### 実装内容

- （変更したファイル、追加したコンポーネント、直面した課題などを具体的に記述）
