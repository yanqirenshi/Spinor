# Skill: update-manual-sitemap

`docs/` 配下のドキュメントサイトマップを `MANUAL.md` に反映するスキルです。

## トリガー条件

以下の場合にこのスキルを実行してください：

1. `docs/` 配下にファイルが **追加** された場合
2. `docs/` 配下のファイルが **削除** された場合
3. `docs/` 配下のファイルが **リネーム/移動** された場合
4. ユーザーが `/update-manual-sitemap` を実行した場合

## 実行手順

1. `docs/` 配下の現在のファイル構造を取得する
   ```
   Glob: docs/**/*
   ```

2. `MANUAL.md` を読み込み、現在のサイトマップを確認する

3. 差分を特定する：
   - 新規追加されたファイル
   - 削除されたファイル
   - 移動/リネームされたファイル

4. `MANUAL.md` のサイトマップセクションを更新する：
   - 新規ファイルは適切なカテゴリに追加
   - 削除されたファイルはサイトマップから削除
   - 統計情報を更新

## カテゴリ分類ルール

`docs/docs/ref/` 配下のファイルは以下のカテゴリに分類：

| カテゴリ | 対象 |
|----------|------|
| 基本操作 | def, define, fn, mac, let, setq, quote, if, begin, progn, match, data |
| リスト操作 | cons, car, cdr, list, empty-p, nil-p |
| 算術演算 | add, sub, mul, mod, inverse |
| 比較演算 | eq, eq-op, equal, lt, gt, string-eq |
| 文字列操作 | string-* 系, list-to-string |
| ファイルI/O | read-file, write-file, append-file, file-exists-p |
| 並行処理 | spawn, sleep, *-mvar |
| パッケージ | defpackage, in-package, current-package, export, use-package |
| 例外処理 | handler-case, ignore-errors, unwind-protect |
| JSON | json-* 系 |
| 行列演算 | matrix, m-*, mdim, mref, transpose, inverse |
| GPU/OpenCL | cl-*, to-device, to-host |
| OpenGL | gl-* 系 |
| 出力 | print |

`docs/docs/syntax/` 配下のファイル：

| ファイル | 説明 |
|----------|------|
| arrays.md | 配列 |
| atoms.md | アトム |
| characters.md | 文字 |
| conditions.md | コンディション |
| conses.md | コンスセル |
| control-flow.md | 制御フロー |
| data-types.md | データ型 |
| definitions.md | 定義 |
| environment.md | 環境 |
| evaluation.md | 評価 |
| files.md | ファイル |
| iteration.md | 反復 |
| numbers.md | 数値 |
| ownership.md | 所有権 |
| packages.md | パッケージ |
| regions.md | リージョン |
| strings.md | 文字列 |
| symbols.md | シンボル |
| type-system.md | 型システム |

新規ファイルが既存カテゴリに該当しない場合は、適切な新カテゴリを作成してください。

## 出力例

変更完了後、以下の形式で報告：

```
MANUAL.md を更新しました:
- 追加: docs/docs/ref/new-function.md (カテゴリ: 基本操作)
- 削除: docs/docs/ref/old-function.md
- 統計: ref/ 60 → 61 ファイル
```
