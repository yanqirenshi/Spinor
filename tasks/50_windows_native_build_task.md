# Task 50: Windows Native Build Foundation の実装

Windows 環境でのビルドエラーを解消し、CI での自動ビルドを有効化してください。

## ステップ

### 1. [User Task] ローカル環境の整備 (MSYS2)
※これはユーザー（人間）が自身の Windows 環境で行う作業です。
1. MSYS2 MinGW64 ターミナルを開きます。
2. 以下のコマンドを実行してビルドツールと OpenBLAS をインストールします：
   ```bash
   pacman -S --noconfirm base-devel autoconf automake make pkgconf 
          mingw-w64-x86_64-openblas mingw-w64-x86_64-glfw 
          mingw-w64-x86_64-toolchain
   ```

### 2. [Engineer Task] Cabal 設定の作成
- プロジェクトルートに `cabal.project.local` を作成し、以下の内容を書き込んでください。
  ```cabal
  package hmatrix
    flags: +openblas
  ```
- これにより、`hmatrix` がシステム上の OpenBLAS を利用するようになります。

### 3. [Engineer Task] CI ワークフローの改修
- `.github/workflows/release.yml` を編集してください。
- `jobs` 内の Windows ジョブ（`os: windows-latest` を含むもの）に、MSYS2 のセットアップステップを追加してください。
- 実行例:
  ```yaml
  - name: Setup MSYS2
    uses: msys2/setup-msys2@v2
    with:
      msystem: MINGW64
      update: true
      install: >-
        mingw-w64-x86_64-openblas
        mingw-w64-x86_64-pkg-config
        base-devel
        autoconf
        automake
  ```

### 4. [User Task] 動作確認
- PowerShell 等で `cabal clean` を実行。
- `cabal build` および `cabal test` を実行し、`network` と `hmatrix` のコンパイルが通り、行列演算のテストがパスすることを確認してください。

## 実装報告ルール
実装完了後、**このファイル自体を編集して**、以下のセクションを末尾に追記してください。

### 実装方針
(GitHub Actions での MSYS2 セットアップの工夫や、cabal.project.local の採用理由について)

### 実装内容
(変更したファイルの一覧、追加した CI ステップの詳細など)
