# Skill: generate-tasks

GitHub Projects (v2) からタスクリストを取得し、`TODO_new.md` と同じスタイルで `TASKS.md` に出力するスキルです。

## トリガー条件

以下の場合にこのスキルを実行してください：

1. ユーザーが `/generate-tasks` を実行した場合
2. ユーザーが「タスクリストを更新して」等を依頼した場合

## 対象プロジェクト

以下の GitHub Projects からタスクを取得します。セクション番号は `TODO_new.md` のエピック番号と一致させます。

| Section | No. | Project URL | 説明文 (TODO_new.md 由来) |
|---------|-----|-------------|---------------------------|
| (0) | 39 | https://github.com/users/yanqirenshi/projects/39 | Spinor プロジェクト全体に関わる横断的タスク。 |
| (1) | 40 | https://github.com/users/yanqirenshi/projects/40 | 既存の Lisp エコシステムとの親和性を高め、言語としての表現力と実用性を向上させる。 |
| (2) | 41 | https://github.com/users/yanqirenshi/projects/41 | LLVM への移行やハードウェア API との連携を通じて、実行性能を極限まで引き出す環境を構築する。 |
| (3) | 42 | https://github.com/users/yanqirenshi/projects/42 | 言語が「実世界」とやり取りするためのインターフェースの構築。 |
| (4) | 43 | https://github.com/users/yanqirenshi/projects/43 | エコシステムの検証とドッグフーディングを兼ねた、実用的なソフトウェアの構築。 |
| (5) | 44 | https://github.com/users/yanqirenshi/projects/44 | 開発者が Spinor を発見し、迷わず使い始めるための環境整備。 |
| (6) | 45 | https://github.com/users/yanqirenshi/projects/45 | 言語コアやライブラリの信頼性を担保し、安全な開発サイクルを回すためのテスト基盤の強化。 |
| (7) | 46 | https://github.com/users/yanqirenshi/projects/46 | Spinor を単なるアプリケーションレベルの処理系から、ハードウェアと直接対話する「Lispマシン (OS / ハードウェア)」へと昇華させる。 |

セクションタイトル (TODO_new.md と完全一致):
- (0) `全体`
- (1) `Common Lisp Alignment (CL互換レイヤー)`
- (2) `コンパイラの進化とネイティブ基盤 (Compiler Evolution & Native Foundation)`
- (3) `ライブラリの充実 (Standard Library & Ecosystem)`
- (4) `アプリケーションの構築 (Real-world Applications)`
- (5) `利用促進 (Promotion & Developer Experience)`
- (6) `テストの充実 (Testing & Quality Assurance)`
- (7) `究極の目標: Lispマシンの創生 (The Ultimate Dream)`

## 実行手順

1. 各プロジェクトの情報を取得する
   ```bash
   gh project view {NUMBER} --owner yanqirenshi --format json
   ```

2. 各プロジェクトのアイテム (Issue) を取得する
   ```bash
   gh project item-list {NUMBER} --owner yanqirenshi --format json
   ```

3. 各 Issue の本文 (description) を取得する
   ```bash
   gh issue view {NUMBER} --repo yanqirenshi/Spinor --json body
   ```
   - body の **1 行目 (空行・見出し記号を除く最初の非空テキスト行)** を抽出する
   - body が空の場合は `_(description 未設定)_` と表示する

4. 取得したデータをプロジェクトごと・ステータスごとにグループ化する

5. `TASKS.md` に `TODO_new.md` スタイルで出力する (詳細は下記の出力フォーマット参照)

## ステータス → チェックボックスのマッピング

| GitHub Status | Checkbox | 補足 |
|---------------|----------|------|
| `Done`        | `- [x]`  | 完了 |
| `In progress` | `- [ ] 🚧` | 進行中 (絵文字併記) |
| `Todo`        | `- [ ]`  | 未着手 |
| `Backlog`     | `- [ ]`  | 未着手 |
| `Ready`       | `- [ ]`  | 未着手 |
| `In review`   | `- [ ]`  | 未着手 |
| その他         | `- [ ]`  | 未着手扱い |

## 出力フォーマット

```markdown
# Spinor Tasks (Auto-generated)

> このファイルは `/generate-tasks` スキルによって GitHub Projects から自動生成されています。
> 直接編集せず、Issue / Project ボードを更新してから再生成してください。
> ロードマップ (高レベルビジョン) は `TODO_new.md` を参照してください。

## (0) 全体

Spinor プロジェクト全体に関わる横断的タスク。

- [x] **[Issue タイトル #1](https://github.com/yanqirenshi/Spinor/issues/1)**
    - Issue description の 1 行目
- [ ] 🚧 **[Issue タイトル #N](https://github.com/yanqirenshi/Spinor/issues/N)**
    - Issue description の 1 行目

## (1) Common Lisp Alignment (CL互換レイヤー)

既存の Lisp エコシステムとの親和性を高め、言語としての表現力と実用性を向上させる。

- [x] **[Issue タイトル #N](issue URL)**
    - Issue description の 1 行目

(...セクション (2)〜(7) も同様...)
```

### フォーマットルール

- 各セクションのヘッダ: `## (N) セクションタイトル` (`TODO_new.md` と一致)
- ヘッダ直下に空行 1 行 → 上記表の手書き説明文 → 空行 1 行 → Issue リスト
- Issue 行: `- {checkbox} **[Issue タイトル #番号](Issue URL)**`
- Issue 行の直下にインデント (4 スペース) で description の 1 行目を `- {本文1行目}` として記載
- 同一 Issue が複数プロジェクトに存在する場合は、それぞれのセクションに表示する

### Issue のソート順

各セクション内で次の順に並べる:
1. **In progress** (🚧)
2. **Todo / Ready / In review**
3. **Backlog**
4. **Done**

同ステータス内では Issue 番号の昇順とする。

## 既存ファイルの扱い

- `TASKS.md` が既に存在する場合は **完全に上書き** する (差分マージは行わない)
- ファイル冒頭の自動生成警告コメント (`> このファイルは...`) を必ず含める

## 権限エラー時の対応

`gh` コマンドで `read:project` スコープが不足している場合：

```bash
gh auth refresh -s read:project
```

を実行するようユーザーに案内してください。

## 完了報告フォーマット

実行完了後、以下の形式で報告：

```
TASKS.md を更新しました:
- 全 8 プロジェクトから {ユニーク Issue 数} 件の Issue を取得
- 🚧 In progress: N 件
- ⬜ Todo / Backlog / Ready / In review: N 件
- ✅ Done: N 件
```
