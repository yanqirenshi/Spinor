# Step 34: Binary Distribution - 実装指示書

## 概要
GitHub Actions を設定し、タグプッシュ時に自動的にマルチプラットフォームのバイナリをリリースするようにしてください。

## Steps

### 1. ワークフロー定義の作成
- `.github/workflows/release.yml` を作成する。
- 以下の YAML 構造をベースに実装してください。

```yaml
name: Release

on:
  push:
    tags:
      - 'v*'
  workflow_dispatch:

jobs:
  build:
    name: Build on ${{ matrix.os }}
    runs-on: ${{ matrix.os }}
    strategy:
      matrix:
        os: [ubuntu-latest, windows-latest, macos-latest]
        include:
          - os: ubuntu-latest
            artifact_name: spinor-linux
          - os: windows-latest
            artifact_name: spinor-windows.exe
          - os: macos-latest
            artifact_name: spinor-macos

    steps:
      - uses: actions/checkout@v4
      - uses: haskell/actions/setup@v2
        with:
          ghc-version: '9.6.3'
          cabal-version: 'latest'

      - name: Cache dependencies
        uses: actions/cache@v3
        with:
          path: ${{ runner.os == 'Windows' && 'AppData/Roaming/cabal/store' || '~/.cabal/store' }}
          key: ${{ runner.os }}-cabal-${{ hashFiles('**/*.cabal') }}

      - name: Build
        run: |
          cabal update
          cabal build exe:spinor -O2 --enable-executable-stripping

      - name: Prepare binary
        shell: bash
        run: |
          # バイナリのパスを特定し、リネームしてルートへ移動
          BIN_PATH=$(cabal list-bin exe:spinor)
          cp "$BIN_PATH" "${{ matrix.artifact_name }}"

      - name: Release
        uses: softprops/action-gh-release@v1
        if: startsWith(github.ref, 'refs/tags/')
        with:
          files: ${{ matrix.artifact_name }}
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
```

### 2. 注意事項
- **Cabal バージョン:** `cabal list-bin` は Cabal 3.8 以降の機能です。セットアップ時にバージョンを確認してください。
- **Windows の Shell:** `Prepare binary` ステップでは、Windows でも共通のロジックが使えるよう `shell: bash` を指定してください。
- **権限:** `GITHUB_TOKEN` にリリースの書き込み権限が必要です（デフォルトで付与されているはずですが、リポジトリの設定を確認してください）。

### 3. 検証手順
- タグを打たずに、GitHub のウェブ画面から Actions ページへ行き、`workflow_dispatch` でワークフローを手動実行する。
- 3つのジョブが全て成功し、ビルドされたバイナリが正しく生成されていることをログから確認する。
- (可能であれば) 実際に `v0.1.0-rc1` 等のテストタグをプッシュして、GitHub の Releases ページにバイナリがアップロードされることを確認する。

---
## 実装報告

### Implementation Policy (実装方針)
仕様書のテンプレートをベースに、より堅牢で保守性の高いワークフローを構築した。特に、ビルドとリリースのジョブを分離し、全プラットフォームのビルドが成功してから一括でリリースする構成を採用。

### Implementation Details (実装内容)

#### 作成ファイル
- `.github/workflows/release.yml`

#### テンプレートからの改良点

1. **ジョブ構造の改善**
   - ビルドジョブ (`build`) とリリースジョブ (`release`) を分離
   - `needs: build` により、全3プラットフォームのビルド成功を待ってからリリース
   - 各ビルドジョブでは `actions/upload-artifact@v4` でアーティファクトをアップロード
   - リリースジョブで `actions/download-artifact@v4` により一括取得

2. **キャッシュの最適化**
   - OSごとのキャッシュパスを `matrix.cache_path` で定義（GitHub Actions の式では三項演算子が使えないため）
   - `dist-newstyle` ディレクトリもキャッシュ対象に追加し、ビルド時間を短縮
   - `restore-keys` を追加して部分的なキャッシュヒットも活用

3. **アクションの更新**
   - `actions/cache@v3` → `actions/cache@v4`
   - `softprops/action-gh-release@v1` → `softprops/action-gh-release@v2`
   - `haskell/actions/setup@v2` → `haskell-actions/setup@v2` (正式な org 名)

4. **リリース設定の強化**
   - `generate_release_notes: true` を追加し、リリースノートを自動生成
   - `permissions: contents: write` を明示的に追加

5. **バイナリ抽出方法の変更** (実行時エラーにより修正)
   - `cabal list-bin` → `cabal install --installdir=. --install-method=copy` に変更
   - キャッシュされた `dist-newstyle` とビルド成果物の不整合問題を回避
   - `--enable-executable-stripping` を削除（互換性のため）

#### 検証
- `v0.1.0-rc2` タグで全3プラットフォーム (Linux, Windows, macOS) のビルドとリリースを確認済み
