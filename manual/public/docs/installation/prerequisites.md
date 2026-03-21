# 共通の前提条件

Spinor のビルドには Haskell ツールチェーン (GHC, Cabal) が必要です。
[GHCup](https://www.haskell.org/ghcup/) を使用したインストールを推奨します。

## 必要なバージョン

| ツール | 最小バージョン | 推奨バージョン |
|:-------|:---------------|:---------------|
| GHC | 9.6.0 | 9.6.7 |
| Cabal | 3.10.0 | 3.10.3 |
| Git | 2.0 | 最新版 |

---

## Windows 11 (PowerShell)

### GHCup のインストール

PowerShell を **管理者として実行** し、以下のコマンドを実行します:

```powershell
winget install -e --id Haskell.GHCup
```

### 対話プロンプトへの回答

インストール中に以下の質問が表示されます。すべて `Y` を選択することを推奨します:

```
Do you want to install the haskell-language-server (HLS)?
→ Y (エディタ補完・型情報表示に必要)

Do you want to install Stack?
→ Y (一部のプロジェクトで使用)

Do you want to create a desktop shortcut?
→ Y (任意)

Do you want to install MSys2 toolchain?
→ Y (フル機能ビルドに必要。hmatrix, OpenGL, OpenCL を使用する場合は必須)
```

> **Note:** MSys2 は [Windows (MSYS2)](windows-msys2) ガイドで説明しているフル機能ビルドに必要です。
> 最小構成のみで良い場合は `N` でも構いません。

### ターミナルの再起動

インストール完了後、**PowerShell を閉じて新しいウィンドウを開いてください**。
環境変数 (PATH) が反映され、`ghc` や `cabal` コマンドが使用可能になります。

---

## Linux / macOS / WSL2

### GHCup のインストール

ターミナルで以下のコマンドを実行します:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

画面の指示に従い、HLS (haskell-language-server) と Stack もインストールすることを推奨します。

### シェルの再起動

インストール完了後、**ターミナルを閉じて新しいウィンドウを開く**か、以下を実行してください:

```bash
source ~/.ghcup/env
```

---

## バージョン確認

インストール後、以下のコマンドでバージョンを確認できます:

```bash
ghc --version
cabal --version
git --version
```

正常にインストールされていれば、各ツールのバージョンが表示されます。
