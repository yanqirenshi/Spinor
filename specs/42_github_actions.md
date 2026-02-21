# Spec 42: GitHub Actions Integration for Manual Site

## 1. Overview

React で構築したマニュアルサイト (`manual/` ディレクトリ) のビルドとデプロイを自動化する。
`main` ブランチへのプッシュをトリガーとして GitHub Actions ワークフローを実行し、ビルド成果物をリポジトリの `docs/` ディレクトリに配置・コミットする。これにより、GitHub Pages への公開が自動的に行われる CI/CD パイプラインを構築する。

## 2. Workflow Definition

- **Workflow Path:** `.github/workflows/deploy-docs.yml`
- **Workflow Name:** `Deploy Manual to GitHub Pages`
- **Trigger:** `main` ブランチへの `push` イベント。

## 3. Job Steps

ワークフローは単一のジョブ `build-and-deploy` で構成され、以下のステップを実行する。

- **Environment:** `ubuntu-latest` ランナーを使用する。

1.  **Checkout Code:**
    - `actions/checkout@v4` を使用して、リポジトリの最新のコードを取得する。

2.  **Setup Node.js:**
    - `actions/setup-node@v4` を使用して、Node.js 環境 (v20) をセットアップする。

3.  **Build React Application:**
    - `manual/` ディレクトリに移動し、`npm install` で依存関係をインストール後、`npm run build` で静的サイトをビルドする。ビルド成果物は `manual/dist/` に生成される。

4.  **Deploy to `docs/` Directory:**
    - `docs/` ディレクトリ内の既存のファイルをクリーンアップする。これにより、古いビルド成果物が残らないようにする。
    - `manual/dist/` ディレクトリ内の全てのビルド成果物 (`index.html`, `assets/` など) を `docs/` ディレクトリにコピーする。
    - GitHub Pages が Jekyll を実行しないように、`docs/.nojekyll` ファイルを作成する。

5.  **Commit and Push Changes:**
    - `stefanzweifel/git-auto-commit-action@v5` などのアクションを利用して、`docs/` ディレクトリへの変更を自動でコミットし、`main` ブランチにプッシュする。
    - コミットは CI スクリプトによる自動更新であることがわかるメッセージ（例: `docs: Auto-deploy manual site`）を付与する。

## 4. Vite Configuration for GitHub Pages

GitHub Pages はリポジトリをサブディレクトリ（例: `https://<user>.github.io/<repo>/`）でホストする。
このため、アセット（CSS, JS, 画像ファイル）へのリンクを正しく解決するには、Vite の設定ファイルでベースパスを明示的に指定する必要がある。

- **File:** `manual/vite.config.ts`
- **Property:** `base` プロパティにリポジトリ名を設定する (例: `base: '/Spinor/'`)。

この設定がない場合、デプロイされたサイトのスタイルやスクリプトが正しく読み込まれない。
