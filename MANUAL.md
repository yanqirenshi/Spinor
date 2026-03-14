# Spinor Documentation Sitemap

`docs/` ディレクトリ配下のドキュメント構成です。

```
docs/
├── index.html                    # メインエントリーポイント
├── 404.html                      # 404エラーページ
├── .nojekyll                     # GitHub Pages Jekyll無効化
├── vite.svg                      # Viteロゴ
│
├── assets/                       # 静的アセット
│   ├── spinor-logo.png
│   ├── index-BSpS4nHS.js
│   └── index-CWJFlKEm.css
│
└── docs/                         # ドキュメント本体
    │
    ├── [概要・ガイド]
    │   ├── introduction.md       # イントロダクション
    │   ├── build.md              # ビルド方法
    │   ├── architecture.md       # アーキテクチャ
    │   ├── syntax.md             # 構文概要
    │   ├── cookbook.md           # クックブック
    │   ├── api-index.md          # APIインデックス
    │   ├── emacs_setup.md        # Emacsセットアップ
    │   ├── ai_workflow.md        # AIワークフロー
    │   └── sample.md             # サンプル
    │
    ├── syntax/                   # 構文詳細
    │   ├── arrays.md             # 配列
    │   ├── atoms.md              # アトム
    │   ├── characters.md         # 文字
    │   ├── conditions.md         # コンディション
    │   ├── conses.md             # コンスセル
    │   ├── control-flow.md       # 制御フロー
    │   ├── data-types.md         # データ型
    │   ├── definitions.md        # 定義
    │   ├── environment.md        # 環境
    │   ├── evaluation.md         # 評価
    │   ├── files.md              # ファイル
    │   ├── iteration.md          # 反復
    │   ├── numbers.md            # 数値
    │   ├── ownership.md          # 所有権
    │   ├── packages.md           # パッケージ
    │   ├── regions.md            # リージョン
    │   ├── strings.md            # 文字列
    │   ├── symbols.md            # シンボル
    │   └── type-system.md        # 型システム
    │
    └── ref/                      # APIリファレンス
        │
        ├── [基本操作]
        │   ├── def.md / define.md    # 定義
        │   ├── fn.md                 # 関数定義
        │   ├── mac.md                # マクロ定義
        │   ├── let.md                # ローカル束縛
        │   ├── setq.md               # 代入
        │   ├── quote.md              # クォート
        │   ├── if.md                 # 条件分岐
        │   ├── begin.md / progn.md   # 逐次実行
        │   ├── match.md              # パターンマッチ
        │   └── data.md               # データ定義
        │
        ├── [リスト操作]
        │   ├── cons.md               # cons
        │   ├── car.md / cdr.md       # car/cdr
        │   ├── list.md               # list
        │   ├── empty-p.md            # empty?
        │   └── nil-p.md              # nil?
        │
        ├── [算術演算]
        │   ├── add.md                # +
        │   ├── sub.md                # -
        │   ├── mul.md                # *
        │   ├── mod.md                # mod
        │   └── inverse.md            # 逆数
        │
        ├── [比較演算]
        │   ├── eq.md / eq-op.md      # eq
        │   ├── equal.md              # equal
        │   ├── lt.md / gt.md         # < / >
        │   └── string-eq.md          # string=
        │
        ├── [文字列操作]
        │   ├── string-append.md      # 文字列連結
        │   ├── string-length.md      # 文字列長
        │   ├── substring.md          # 部分文字列
        │   ├── string-to-list.md     # 文字列→リスト
        │   └── list-to-string.md     # リスト→文字列
        │
        ├── [ファイルI/O]
        │   ├── read-file.md          # ファイル読込
        │   ├── write-file.md         # ファイル書込
        │   ├── append-file.md        # ファイル追記
        │   └── file-exists-p.md      # ファイル存在確認
        │
        ├── [並行処理]
        │   ├── spawn.md              # スレッド生成
        │   ├── sleep.md              # スリープ
        │   ├── new-mvar.md           # MVar生成
        │   ├── put-mvar.md           # MVar書込
        │   └── take-mvar.md          # MVar読込
        │
        ├── [パッケージ]
        │   ├── defpackage.md         # パッケージ定義
        │   ├── in-package.md         # パッケージ切替
        │   ├── current-package.md    # 現在パッケージ
        │   ├── export.md             # エクスポート
        │   └── use-package.md        # パッケージ使用
        │
        ├── [例外処理]
        │   ├── handler-case.md       # ハンドラケース
        │   ├── ignore-errors.md      # エラー無視
        │   └── unwind-protect.md     # アンワインド保護
        │
        ├── [JSON]
        │   ├── json-parse.md         # JSONパース
        │   └── json-stringify.md     # JSON文字列化
        │
        ├── [行列演算]
        │   ├── matrix.md             # 行列生成
        │   ├── m-add.md / m-mul.md   # 行列加算/乗算
        │   ├── mdim.md / mref.md     # 次元/参照
        │   └── transpose.md          # 転置
        │
        ├── [GPU/OpenCL]
        │   ├── cl-init.md            # OpenCL初期化
        │   ├── cl-compile.md         # カーネルコンパイル
        │   ├── cl-enqueue.md         # キュー投入
        │   ├── to-device.md          # デバイス転送
        │   └── to-host.md            # ホスト転送
        │
        ├── [OpenGL]
        │   ├── gl-init.md            # OpenGL初期化
        │   ├── gl-clear.md           # クリア
        │   ├── gl-draw-points.md     # 点描画
        │   ├── gl-swap-buffers.md    # バッファスワップ
        │   └── gl-window-should-close.md
        │
        └── [出力]
            └── print.md              # 出力
```

## 統計

| カテゴリ | ファイル数 |
|----------|------------|
| 概要・ガイド | 9 |
| syntax/ | 19 |
| ref/ | 70 |
| **合計** | **98** |
