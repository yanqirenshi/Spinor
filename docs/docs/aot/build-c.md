# build コマンド (C 経由)

C 言語にトランスパイルし、GCC/Clang でネイティブバイナリを生成します。

## 使用方法

```bash
# hello.spin をネイティブバイナリにコンパイル
cabal run spinor -- build hello.spin

# 生成された実行ファイルを実行
./hello        # Linux/macOS
./hello.exe    # Windows
```

## `--emit-c`: 中間 C コードのみ生成 (コンパイルをスキップ)

C コードのみを出力し、`gcc` / `clang` の呼び出しをスキップする場合は `--emit-c` フラグを使用します。

```bash
# hello.spin → hello.c (C ソース) のみを生成 (バイナリは作成されない)
cabal run spinor -- build --emit-c hello.spin
# または:
cabal run spinor -- build hello.spin --emit-c
```

### 動作の詳細

- 生成された C コードは **カレントディレクトリ** に `<元のファイル名（拡張子なし）>.c` として書き出されます。
  - 例: `examples/foo.spin --emit-c` → `./foo.c` (`examples/` 配下ではなく cwd 直下)
- 通常モード (フラグなし) との違い:
  | 項目 | 通常モード | `--emit-c` モード |
  |------|----------|-----------------|
  | C コード生成 | ✅ 行う | ✅ 行う |
  | 出力先 | 元ファイルの隣 (中間) | cwd 直下 (永続) |
  | gcc/clang 呼び出し | ✅ 行う | ❌ スキップ |
  | 中間 C ファイルの削除 | ✅ ビルド成功時に削除 | ❌ 保持 |
  | 最終成果物 | ネイティブバイナリ | C ソース |

### ユースケース

- **Unikernel ビルド**: [Unikraft](https://unikraft.org/) などの外部 C ツールチェインに生成物を渡し、ベアメタルで動かす Unikernel イメージを構築する場合。詳細は [Unikernel Architecture](../vision/unikernel-architecture.md) を参照。
- **C コードの検査・デバッグ**: 生成された C を読んで意味論を確認したり、別の C コンパイラ (Tiny C Compiler 等) でビルドしたい場合。
- **クロスコンパイル**: 生成された C ソースを別環境 (組み込みデバイス、別 OS など) に持っていって現地でビルドする場合。

### 既存の `compile` サブコマンドとの違い

`spinor compile <file>` も C コードのみを出力しますが、出力先が常に `output.c` 固定である点が異なります。`build --emit-c` は元のファイル名を反映した命名規則 (`<basename>.c`) を採用しているため、複数のソースを連続でコンパイルする場合に上書き衝突を避けられます。

## 前提条件

GCC または Clang がシステムにインストールされている必要があります。

### Windows

```powershell
# MSYS2 UCRT64 環境で GCC をインストール
pacman -S mingw-w64-ucrt-x86_64-gcc
```

### Linux / WSL2

```bash
# Ubuntu / Debian
sudo apt update
sudo apt install build-essential
```

### macOS

```bash
# Xcode Command Line Tools
xcode-select --install
```
