# Region-based Memory (Experimental)

> **注意**: この機能は実験的 (Experimental) です。将来のバージョンで API が変更される可能性があります。

リージョンベースのメモリ管理は、オブジェクトを「リージョン（アリーナ）」と呼ばれるメモリ領域にまとめて割り当て、スコープ終了時に一括解放する仕組みです。

---

## 概要

### 目的

- **高速なアロケーション**: 小さなオブジェクトを大量に作成する場合、個別の malloc/free よりも効率的
- **確実な解放**: スコープを抜けると自動的にメモリが解放される
- **GC レス**: ガベージコレクションのオーバーヘッドなしでメモリ管理

### 用語

| 用語 | 説明 |
|------|------|
| **Region** | メモリプール。複数のオブジェクトを連続領域に割り当てる |
| **Arena** | Region の別名。アリーナアロケータとも呼ばれる |
| **Escape** | リージョン内のオブジェクトがリージョン外に持ち出されること |

---

## 基本的な使い方

### with-region

`with-region` は新しいリージョンを作成し、そのスコープ内でメモリを管理します。

```lisp
;; リージョン r を作成し、body を評価
(with-region r
  ;; この中で r を使ったメモリ割り当てが可能
  (process-data r))
;; ここでリージョン r のメモリは自動的に解放される
```

### alloc-in

`alloc-in` は指定したリージョンにオブジェクトを割り当てます。

```lisp
(with-region r
  ;; 整数 42 をリージョン r に割り当て
  (let ((x (alloc-in r 42)))
    ;; 文字列もリージョンに割り当て
    (let ((s (alloc-in r "hello")))
      (process x s))))
;; x と s はここで自動解放される
```

---

## 逃避解析 (Escape Analysis)

リージョン内で割り当てられたオブジェクトは、リージョン外に持ち出すことができません。
コンパイラは静的解析によってこれを検証します。

### 禁止されるパターン

```lisp
;; エラー例 1: リージョン内の値を return
(defun bad-example ()
  (with-region r
    (alloc-in r 42)))  ; エラー: リージョンの値が関数から逃避

;; エラー例 2: 未定義リージョンへの割り当て
(alloc-in undefined-region 42)  ; エラー: リージョン 'undefined-region' は未定義
```

### 許可されるパターン

```lisp
;; OK: リージョン内で値を使い切る
(with-region r
  (let ((data (alloc-in r (generate-data))))
    (print data)
    ;; data はリージョン内で消費される
    (process data)))

;; OK: 結果をリージョン外の値として返す
(with-region r
  (let ((temp (alloc-in r (parse input))))
    ;; temp を使って計算し、結果（リージョン外）を返す
    (compute-result temp)))  ; compute-result の戻り値はリージョン外
```

---

## C コード生成

`with-region` は C のブロックスコープに展開され、リージョンの初期化と破棄が自動挿入されます。

### 入力 (Spinor)

```lisp
(with-region r
  (let ((x (alloc-in r 42)))
    (print x)))
```

### 出力 (C)

```c
{ /* with-region r */
    Region* user_r = create_region();

    SpObject* user_x = sp_region_make_int(user_r, 42);
    sp_print(user_x);

    destroy_region(user_r);
}
```

---

## Arena アロケータの実装

C ランタイムには以下の構造体と関数が追加されます。

### データ構造

```c
typedef struct RegionBlock {
    struct RegionBlock* next;
    size_t size;
    size_t used;
    char data[];  /* フレキシブル配列メンバ */
} RegionBlock;

typedef struct Region {
    RegionBlock* head;
    RegionBlock* current;
} Region;
```

### API

```c
/* 新しいリージョンを作成 */
Region* create_region(void);

/* リージョンからメモリを割り当て */
void* region_alloc(Region* r, size_t size);

/* リージョンを破棄（全メモリを解放） */
void destroy_region(Region* r);

/* リージョン内に SpObject を作成 */
SpObject* sp_region_alloc(Region* r);
SpObject* sp_region_make_int(Region* r, int64_t n);
SpObject* sp_region_make_str(Region* r, const char* s);
```

---

## 所有権システムとの関係

リージョンベースのメモリ管理は、[Linear Types & Ownership](ownership) と組み合わせて使用できます。

| 機能 | 所有権システム | リージョン |
|------|--------------|----------|
| **粒度** | 個別オブジェクト | オブジェクト群 |
| **解放タイミング** | 使用終了時 | スコープ終了時 |
| **ユースケース** | 単一リソース管理 | 一時データの大量生成 |

### 使い分け

- **所有権**: ファイルハンドル、ネットワーク接続など、個別に管理が必要なリソース
- **リージョン**: パース結果、一時的な計算結果など、まとめて解放できるデータ

---

## 制限事項

現在のプロトタイプ実装には以下の制限があります：

1. **入れ子リージョン**: 内側のリージョンから外側のリージョンへの参照は未検証
2. **クロージャ**: クロージャがキャプチャしたリージョン変数の追跡は未サポート
3. **条件分岐**: `if` 式の両分岐でのリージョン使用状態の統合は簡略化
4. **動的リージョン**: リージョン名は静的に決定される必要あり

---

## EscapeAnalysis モジュール

逃避解析は `Spinor.EscapeAnalysis` モジュールで実装されています。

### API

```haskell
-- 逃避解析の実行
checkEscape :: [Expr] -> EscapeResult

-- 結果
data EscapeResult = EscapeResult
  { erErrors  :: [EscapeError]   -- 検出されたエラー
  , erRegions :: Set Text        -- 使用されたリージョン名
  }

-- エラーの種類
data EscapeError
  = EscapeReturn Text Text SourceSpan     -- 関数からの逃避
  | EscapeAssign Text Text Text SourceSpan -- 外部変数への代入
  | UndefinedRegion Text SourceSpan        -- 未定義リージョン
```

---

## 関連ドキュメント

- [Linear Types & Ownership](ownership) - 所有権システム
- [Control Flow](control-flow) - 条件分岐と制御構文
- [Architecture](../architecture) - 内部アーキテクチャ
