# 55: Claude Code Skills Integration (Custom Skills) Specification

## 1. 概要
Claude Code 等の AI エージェントが、Spinor のコード修正後に自律的に品質検証（構文チェック、型チェック、テスト実行）を行い、エラーを正確に把握して自己修正ループを回せるように、CLI の出力機能を拡張する。

## 2. CLI 拡張仕様

### 2.1 `--json` グローバルフラグ
- **機能:** 全てのコマンドの出力を、人間向けのテキスト形式から機械可読な JSON 形式に切り替える。
- **挙動:** 指定時、エラーメッセージは標準エラー出力ではなく標準出力に JSON 行として出力される。

### 2.2 `spinor check` コマンド
- **用法:** `spinor check <file>`
- **機能:** 指定されたファイルのパースおよび型推論（静的解析）のみを実行する。評価（実行）や C コードの生成は行わない。
- **目的:** AI が修正したコードの型安全性を、サイドエフェクトなしで高速に検証するため。

## 3. JSON 出力フォーマット

### 3.1 成功時
```json
{
  "status": "success",
  "command": "check",
  "message": "Type check passed. Total X expressions analyzed."
}
```

### 3.2 エラー発生時 (SpinorError)
```json
{
  "status": "error",
  "errors": [
    {
      "file": "src/main.spin",
      "line": 12,
      "col": 10,
      "message": "Type mismatch: Expected Int but got String",
      "code": "TYPE_ERROR"
    }
  ]
}
```

## 4. AI ワークフローの統合 (`CLAUDE.md`)
AI エージェントに対し、以下のワークフローを強制する指示を `CLAUDE.md` に追加する。
1. コードを修正または新規作成する。
2. `spinor check --json <file>` を実行し、静的な不整合がないか確認する。
3. エラーが出た場合、JSON の `line`, `col`, `message` を元に修正案を再考する。
