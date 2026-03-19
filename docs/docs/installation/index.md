# Build Guide

Spinor のビルドには Haskell ツールチェーンが必要です。
本ガイドでは、主要な OS 環境ごとの詳細なセットアップ手順を解説します。

> **Note:** Spinor v0.1.0 以降、LLVM C++ バインディング (`llvm-hs` 等) への依存が排除されました。
> これにより、Windows 環境でも純粋な Haskell プロジェクトとしてビルド可能です。
> AOT コンパイル機能 (`build-llvm`) を使用する場合のみ、システムに LLVM (Clang) のインストールが必要です。

## 目次

- [共通の前提条件](prerequisites) - Haskell ツールチェーンのセットアップ
- [Linux / WSL2](linux) - Ubuntu / Debian 環境でのビルド
- [Windows (PowerShell)](windows-powershell) - 最小構成でのビルド
- [Windows (MSYS2)](windows-msys2) - フル機能を使用する場合
- [ネイティブコンパイル (AOT)](aot) - スタンドアロン実行ファイルの生成
- [トラブルシューティング](troubleshooting) - よくある問題と解決策
