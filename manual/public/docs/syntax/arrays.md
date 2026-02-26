# Arrays & Matrices

Spinor における行列型と線形代数演算について解説します。

## 行列型 (Matrix)

Spinor は科学計算のために `Matrix` 型を提供しています。内部的には hmatrix ライブラリ (BLAS/LAPACK) を使用しており、高性能な線形代数演算が可能です。

## 行列の作成

### matrix 関数

ネストしたリストから行列を作成します。

```lisp
;; 2x3 行列
(matrix '((1 2 3)
          (4 5 6)))

;; 3x3 単位行列風
(matrix '((1 0 0)
          (0 1 0)
          (0 0 1)))
```

## 行列の情報

### 次元の取得

```lisp
(def m (matrix '((1 2 3) (4 5 6))))
(mdim m)          ; => (2 3) (行数, 列数)
```

### 要素へのアクセス

```lisp
(def m (matrix '((1 2 3) (4 5 6))))
(mref m 0 0)      ; => 1 (0行0列)
(mref m 1 2)      ; => 6 (1行2列)
```

## 行列演算

### 加算

```lisp
(def a (matrix '((1 2) (3 4))))
(def b (matrix '((5 6) (7 8))))
(m-add a b)       ; => ((6 8) (10 12))
```

### 乗算

```lisp
(def a (matrix '((1 2) (3 4))))
(def b (matrix '((5 6) (7 8))))
(m-mul a b)       ; => ((19 22) (43 50))
```

### 転置

```lisp
(def m (matrix '((1 2 3) (4 5 6))))
(transpose m)     ; => ((1 4) (2 5) (3 6))
```

### 逆行列

```lisp
(def m (matrix '((1 2) (3 4))))
(inverse m)       ; => 逆行列
```

## GPGPU 連携 (OpenCL)

Spinor は OpenCL を通じて GPU 上での行列計算をサポートしています。

### OpenCL の初期化

```lisp
(cl-init)         ; OpenCL 環境を初期化
```

### デバイスへの転送

```lisp
(def m (matrix '((1 2) (3 4))))
(def dm (to-device m))  ; GPU メモリに転送
```

### ホストへの転送

```lisp
(def result (to-host dm))  ; CPU メモリに戻す
```

### カーネルのコンパイルと実行

```lisp
;; OpenCL カーネルをコンパイル
(def kernel (cl-compile "kernel_source"))

;; カーネルを実行キューに追加
(cl-enqueue kernel args)
```

## 型推論

```lisp
spinor> (matrix '((1 2) (3 4)))
:: Matrix
#<matrix 2x2>

spinor> (m-mul a b)
:: Matrix
#<matrix 2x2>
```

## Symbols

| Type | Name | Description |
|:-----|:-----|:------------|
| Function | [matrix](ref/matrix) | 行列を作成 |
| Function | [mdim](ref/mdim) | 行列の次元を取得 |
| Function | [mref](ref/mref) | 行列の要素を取得 |
| Function | [m-add](ref/m-add) | 行列の加算 |
| Function | [m-mul](ref/m-mul) | 行列の乗算 |
| Function | [transpose](ref/transpose) | 転置行列 |
| Function | [inverse](ref/inverse) | 逆行列 |
| Function | [cl-init](ref/cl-init) | OpenCL 初期化 |
| Function | [cl-compile](ref/cl-compile) | OpenCL カーネルをコンパイル |
| Function | [cl-enqueue](ref/cl-enqueue) | OpenCL カーネルを実行 |
| Function | [to-device](ref/to-device) | GPU メモリに転送 |
| Function | [to-host](ref/to-host) | CPU メモリに転送 |
