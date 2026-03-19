# 共通の前提条件

## Haskell ツールチェーン

[GHCup](https://www.haskell.org/ghcup/) を使用したインストールを推奨します。

```bash
# GHCup のインストール (Linux/macOS/WSL2)
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Windows (PowerShell)
Set-ExecutionPolicy Bypass -Scope Process -Force;
[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072;
try { Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true } catch { Write-Error $_ }
```

必要なバージョン:

| ツール | 最小バージョン | 推奨バージョン |
|:-------|:---------------|:---------------|
| GHC | 9.6.0 | 9.6.7 |
| Cabal | 3.10.0 | 3.10.3 |
| Git | 2.0 | 最新版 |

バージョン確認:

```bash
ghc --version
cabal --version
git --version
```
