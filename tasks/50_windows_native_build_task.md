# Task 50: Windows Native Build Foundation の実装

Windows 環境でのビルドエラーを解消し、CI での自動ビルドを有効化してください。

---

## Windows ビルド & テスト手順

### Step 1: MSYS2 のインストール

1. [MSYS2 公式サイト](https://www.msys2.org/) から最新のインストーラをダウンロード
2. `C:\msys64` にインストール (デフォルト推奨)
3. インストール完了後、MSYS2 MinGW64 ターミナルを起動

### Step 2: 必要パッケージのインストール

MSYS2 MinGW64 ターミナルで以下を実行:

```bash
# パッケージデータベースの更新
pacman -Syu

# ビルドツールのインストール
pacman -S --noconfirm base-devel autoconf automake make pkgconf

# C ライブラリのインストール
pacman -S --noconfirm \
    mingw-w64-x86_64-toolchain \
    mingw-w64-x86_64-openblas \
    mingw-w64-x86_64-glfw \
    mingw-w64-x86_64-opencl-icd \
    mingw-w64-x86_64-opencl-headers
```

### Step 3: システム環境変数の設定

1. Windows 設定 → システム → システムの詳細設定 → 環境変数
2. `Path` に以下を追加:
   ```
   C:\msys64\mingw64\bin
   ```
3. PowerShell を再起動して反映を確認:
   ```powershell
   echo $env:PATH | Select-String "msys64"
   ```

### Step 4: cabal.project.local の作成

プロジェクトルートに `cabal.project.local` を作成:

```cabal
-- Windows (MSYS2/MinGW64) 環境用の設定

package hmatrix
  flags: +openblas
  extra-lib-dirs: C:\msys64\mingw64\lib
  extra-include-dirs: C:\msys64\mingw64\include

package OpenCL
  extra-lib-dirs: C:\msys64\mingw64\lib
  extra-include-dirs: C:\msys64\mingw64\include

package bindings-GLFW
  extra-lib-dirs: C:\msys64\mingw64\lib
  extra-include-dirs: C:\msys64\mingw64\include
```

### Step 5: ビルド

```powershell
cabal build
```

### Step 6: テスト用 DLL のコピー

`cabal test` は PATH を子プロセスに正しく渡さないため、DLL を手動でコピーする必要があります。

```powershell
# テスト実行ディレクトリ
$testDir = "dist-newstyle\build\x86_64-windows\ghc-9.6.7\spinor-0.1.0.0\t\spinor-test\build\spinor-test"

# ディレクトリ作成 (存在しない場合)
New-Item -ItemType Directory -Path $testDir -Force | Out-Null

# DLL コピー
Copy-Item C:\msys64\mingw64\bin\libopenblas.dll $testDir
Copy-Item C:\msys64\mingw64\bin\libgfortran-5.dll $testDir
Copy-Item C:\msys64\mingw64\bin\libquadmath-0.dll $testDir
Copy-Item C:\msys64\mingw64\bin\libgcc_s_seh-1.dll $testDir
Copy-Item C:\msys64\mingw64\bin\libwinpthread-1.dll $testDir
Copy-Item C:\msys64\mingw64\bin\OpenCL.dll $testDir
```

### Step 7: テスト実行

```powershell
cabal test
```

期待される結果: `152 examples, 0 failures`

### 注意事項

- `cabal clean` を実行すると DLL も削除されるため、Step 6 を再実行する必要があります
- GHC バージョンが異なる場合、`$testDir` のパスを適宜変更してください

---

## Engineer Task

### 1. Cabal 設定の作成
- プロジェクトルートに `cabal.project.local` を作成 (上記 Step 4 参照)

### 2. CI ワークフローの改修
- `.github/workflows/release.yml` を編集
- Windows ジョブに MSYS2 セットアップステップを追加:

```yaml
- name: Setup MSYS2
  if: matrix.os == 'windows-latest'
  uses: msys2/setup-msys2@v2
  with:
    msystem: MINGW64
    path-type: inherit
    update: true
    install: >-
      mingw-w64-x86_64-openblas
      mingw-w64-x86_64-pkg-config
      mingw-w64-x86_64-glfw
      mingw-w64-x86_64-opencl-icd
      mingw-w64-x86_64-opencl-headers
      base-devel
      autoconf
      automake
```

---

## 実装報告

### 実装方針

**cabal.project.local の採用理由:**
- ローカル環境固有の設定を分離できる
- Windows では `hmatrix` に `+openblas` フラグと MSYS2 パスが必要
- `cabal.project` とは別に環境依存の設定を管理

**GitHub Actions での MSYS2 セットアップ:**
- `msys2/setup-msys2@v2` を使用し、`MINGW64` サブシステムを指定
- `path-type: inherit` により、MSYS2 のツールが後続ステップから参照可能
- `if: matrix.os == 'windows-latest'` で Windows のみ実行

### 実装内容

**変更ファイル:**

1. **cabal.project.local** (新規作成)
   - hmatrix, OpenCL, bindings-GLFW のパス設定

2. **.github/workflows/release.yml** (編集)
   - MSYS2 セットアップステップを追加

3. **spinor.cabal** (編集)
   - `Spinor.ServerSpec` を `other-modules` に追加

### 動作確認結果

| 項目 | 結果 |
|------|------|
| `cabal build` | 成功 |
| `cabal test` | **152/152 パス** |
