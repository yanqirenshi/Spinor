# Step 34: Binary Distribution (GitHub Actions) - 技術仕様

## 1. 概要
Spinor プロジェクトのリリースプロセスを自動化し、Linux, Windows, macOS の各プラットフォーム向けにビルド済みの実行バイナリを GitHub Release に提供するためのワークフローを定義する。

## 2. GitHub Actions ワークフロー仕様

### 2.1. トリガー
- **Tag Push:** `v*` (例: `v0.1.0`) の形式のタグがプッシュされた際に実行。
- **Manual (Optional):** 開発中のテストのため `workflow_dispatch` を有効にする。

### 2.2. ビルドマトリックス
以下の 3 環境で並列にビルドを実行する。

| OS | Runner | 出力バイナリ名 |
| :--- | :--- | :--- |
| **Linux** | `ubuntu-latest` | `spinor-linux` |
| **Windows** | `windows-latest` | `spinor-windows.exe` |
| **macOS** | `macos-latest` | `spinor-macos` |

### 2.3. ビルド・ジョブの手順 (各OS共通)
1. **Checkout:** `actions/checkout@v4`.
2. **Setup Haskell:** `haskell/actions/setup@v2` を使用。
    - GHC Version: `9.6.3` (または推奨される安定版)
    - Cabal Version: `latest`
3. **Cache:** `actions/cache@v3` を使用して、Haskell の依存関係 (`~/.cabal/store` 等) をキャッシュし、ビルド時間を短縮する。
4. **Build:**
    - `cabal update`
    - `cabal build exe:spinor --enable-executable-stripping -O2`
5. **Artifact Localization:** 
    - `cabal list-bin exe:spinor` (Cabal 3.8+) または `find` コマンド等を用いて、ビルドされたバイナリのパスを特定する。
    - 各プラットフォームの命名規則に合わせてリネームし、作業ディレクトリへ移動。
6. **Upload to Release:**
    - `softprops/action-gh-release@v1` を使用。
    - 前段のビルド成果物をアセットとしてアップロード。
    - タグ名に基づいたリリースノートを自動生成する設定を推奨。

## 3. バイナリの最適化
- **Executable Stripping:** シンボル情報を削除してバイナリサイズを削減する (`--enable-executable-stripping`)。
- **Static Linking (Optional):** 可能であれば Linux 等でスタティックリンクを検討するが、まずは動的リンクのデフォルトビルドで開始する。
