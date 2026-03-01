# 55: Claude Code Skills Integration の実装指示書

## 1. 目的
CLI に `check` コマンドと `--json` オプションを追加し、AI エージェントによる自己修復ループを可能にする。

## 2. 実装手順

### Step 1: JSON 出力基盤の実装
- `src/Spinor/Syntax.hs` の `SpinorError` 等に `ToJSON` インスタンスを定義。
- `aeson` パッケージの利用を確認。

### Step 2: `check` コマンドの実装 (`app/Main.hs`)
- パースと型推論のみを行い、実行しない `checkMode` を追加。

### Step 3: `--json` オプションの統合
- グローバルフラグとして `--json` を処理し、エラー出力を JSON 化する。

### Step 4: `CLAUDE.md` テンプレートの更新
- `src/Spinor/Template.hs` を更新し、AI への自己検証指示を追記。

### Step 5: 検証
- 型エラー時の JSON 出力が AI にとってパース可能であることを確認。

## 3. 完了条件
- `spinor check --json` で型エラーを JSON 報告できること。
- `CLAUDE.md` に検証コマンドの指示が含まれていること。

---
## 実装報告

### 実装方針

AI エージェント (Claude Code 等) がコード修正後に自律的に品質検証を行えるよう、CLI に `check` コマンドと `--json` オプションを追加する。これにより、型エラーを機械可読な JSON 形式で出力し、AI が行・列情報を元に自己修正ループを回せるようになる。

### 実装内容

#### 1. JSON 出力基盤 (`src/Spinor/Syntax.hs`)

**ToJSON インスタンスの追加:**

```haskell
-- SourcePos, SourceSpan, SpinorError に ToJSON インスタンスを定義
instance ToJSON SourcePos where
  toJSON pos = object
    [ "file"   .= posFile pos
    , "line"   .= posLine pos
    , "column" .= posColumn pos
    ]

instance ToJSON SpinorError where
  toJSON err = object
    [ "file"    .= posFile start
    , "line"    .= posLine start
    , "col"     .= posColumn start
    , "message" .= errorMsg err
    , "code"    .= errorCode (errorMsg err)
    ]
```

**エラーコード分類:**
- `UNDEFINED_SYMBOL`: 未定義シンボル
- `TYPE_ERROR`: 型不一致
- `PARSE_ERROR`: パースエラー
- `ARITY_ERROR`: 引数の数エラー
- `ERROR`: その他

#### 2. CLI 拡張 (`app/Main.hs`)

**`--json` グローバルフラグ:**
```haskell
parseJsonFlag :: [String] -> (Bool, [String])
parseJsonFlag args = (hasJson, filter (/= "--json") args)
  where hasJson = "--json" `elem` args
```

**`check` コマンド:**
```haskell
checkMode :: Bool -> FilePath -> IO ()
checkMode jsonOutput file = do
  content <- readFileUtf8 file
  case parseFile content of
    Left err -> outputCheckError jsonOutput [parseErr]
    Right exprs -> do
      (env, tyEnv) <- loadBootQuiet primitiveBindings baseTypeEnv
      checkResult <- checkExprs env tyEnv exprs file
      case checkResult of
        Left errors -> outputCheckError jsonOutput errors
        Right count -> outputCheckSuccess jsonOutput count
```

**JSON 出力関数:**
```haskell
outputCheckSuccess :: Bool -> Int -> IO ()
outputCheckSuccess True count = do
  let result = object
        [ "status"  .= "success"
        , "command" .= "check"
        , "message" .= ("Type check passed. " <> show count <> " expressions analyzed.")
        ]
  BL.putStrLn (encode result)

outputCheckError :: Bool -> [SpinorError] -> IO ()
outputCheckError True errors = do
  let result = object
        [ "status" .= "error"
        , "errors" .= errors
        ]
  BL.putStrLn (encode result)
```

#### 3. cabal 更新 (`spinor.cabal`)

executable に `aeson` 依存関係を追加。

#### 4. Template.hs 更新

`claudeMd` に「AI Development Workflow」セクションを追加:

```markdown
## AI Development Workflow (IMPORTANT)

**When modifying or creating Spinor code, you MUST follow this workflow:**

1. Write or modify the `.spin` file
2. Run `spinor check --json <file>` to verify syntax and type correctness
3. If errors are returned, parse the JSON output and fix issues at the specified line/column
4. Repeat steps 2-3 until no errors remain
```

#### 5. 動作確認

**型エラー時の JSON 出力:**
```bash
$ spinor check --json test_type_error.spin
{"errors":[{"code":"TYPE_ERROR","col":9,"file":"<file>","line":6,"message":"型が一致しません: Int と Str"}],"status":"error"}
```

**成功時の JSON 出力:**
```bash
$ spinor check --json test_valid.spin
{"command":"check","message":"Type check passed. 3 expressions analyzed.","status":"success"}
```

**テキスト形式出力 (--json なし):**
```bash
$ spinor check test_type_error.spin
<file>:6:9: 型が一致しません: Int と Str

$ spinor check test_valid.spin
Type check passed. 3 expressions analyzed.
```

**テスト結果:**
```
229 examples, 0 failures
```

### 完了日
2026-03-01
