# Spinor Tasks (Auto-generated)

> このファイルは `/generate-tasks` スキルによって GitHub Projects から自動生成されています。
> 直接編集せず、Issue / Project ボードを更新してから再生成してください。
> ロードマップ (高レベルビジョン) は `TODO_new.md` を参照してください。

## (0) 全体

Spinor プロジェクト全体に関わる横断的タスク。

- [ ] **[[Feature] CLI 実行時のログレベル制御機能 (-q, --quiet) の追加 #11](https://github.com/yanqirenshi/Spinor/issues/11)**
    - 現在、Spinor スクリプトを実行する際、標準出力に `Loading Twister environment...` などのシステム起動ログが強制的に出力される。
- [x] **[[Refactor] 組み込み関数 `null?` を `nil?` にリネームする #1](https://github.com/yanqirenshi/Spinor/issues/1)**
    - Spinor の組み込み関数 `null?`（空リスト判定）の名称を、より Lisp/Spinor のセマンティクス（空リストを `nil` と表現する文化）に即した `nil?` に変更する。
- [x] **[[Feature] HTTPサーバー基盤構築に向けた低水準 Socket API の実装 #7](https://github.com/yanqirenshi/Spinor/issues/7)**
    - Spinor 上で軽量な HTTP サーバーを構築するための第一歩として、ネットワーク通信の土台となる低水準な TCP Socket API を言語コアに実装する。
- [x] **[[App] はてなブックマーク CLI ツールの強化 (JSON API と `get-in` の活用) #14](https://github.com/yanqirenshi/Spinor/issues/14)**
    - Issue #10 で作成したはてなブックマーク CLI ツール (`app/hatena-b.spin`) を強化する。
- [x] **[[Docs] ビルドガイド (Build Guide) の更新: Windows ネイティブ対応と LLVM 依存の改訂 #21](https://github.com/yanqirenshi/Spinor/issues/21)**
    - LLVM AOT コンパイル機能の実装に伴い、Spinor のビルドプロセスが劇的に簡略化された。

## (1) Common Lisp Alignment (CL互換レイヤー)

既存の Lisp エコシステムとの親和性を高め、言語としての表現力と実用性を向上させる。

- [ ] **[[Feature] Alist およびネストデータ操作ライブラリの拡充 (`alist-get`, `get-in`) #13](https://github.com/yanqirenshi/Spinor/issues/13)**
    - JSON パーサー (`json-parse`) の導入により、Spinor で複雑にネストした Alist (連想リスト) やリストを扱う機会が激増した。
- [x] **[[Research] キーワードシンボル (:XXX) の実装状況の調査と方針決定 #3](https://github.com/yanqirenshi/Spinor/issues/3)**
    - Spinor の現状のカーネル（`Parser.hs`, `Eval.hs` 等）において、コロンから始まるキーワードシンボル（例: `:headers`）がどのようにパース・評価されているかを調査する。
- [x] **[[Feature] キーワード引数 (&key) の構文サポートと評価器への組み込み #4](https://github.com/yanqirenshi/Spinor/issues/4)**
    - Common Lisp 互換性の向上、および今後のライブラリ API（`http-get` など）の設計柔軟性を高めるため、関数定義におけるキーワード引数（`&key`）のサポートを実装する。
- [x] **[[Feature] HTTPサーバー基盤構築に向けた低水準 Socket API の実装 #7](https://github.com/yanqirenshi/Spinor/issues/7)**
    - Spinor 上で軽量な HTTP サーバーを構築するための第一歩として、ネットワーク通信の土台となる低水準な TCP Socket API を言語コアに実装する。
- [x] **[[App] はてなブックマーク CLI ツールの強化 (JSON API と `get-in` の活用) #14](https://github.com/yanqirenshi/Spinor/issues/14)**
    - Issue #10 で作成したはてなブックマーク CLI ツール (`app/hatena-b.spin`) を強化する。
- [x] **[[Docs] ビルドガイド (Build Guide) の更新: Windows ネイティブ対応と LLVM 依存の改訂 #21](https://github.com/yanqirenshi/Spinor/issues/21)**
    - LLVM AOT コンパイル機能の実装に伴い、Spinor のビルドプロセスが劇的に簡略化された。

## (2) コンパイラの進化とネイティブ基盤 (Compiler Evolution & Native Foundation)

LLVM への移行やハードウェア API との連携を通じて、実行性能を極限まで引き出す環境を構築する。

- [ ] **[[Research] LLVM バックエンドの調査と JIT 実行プロトタイプの作成 (Step 36) #9](https://github.com/yanqirenshi/Spinor/issues/9)**
    - Spinor の実行速度向上とネイティブ基盤の確立を目指し、Haskell の `llvm-hs` および `llvm-hs-pure` ライブラリを用いた LLVM 統合の技術調査 (Spike) を行う。
- [ ] **[[Spike] LLVM JIT 拡張: 比較演算と制御構文 (`if`) の実装 (Phase 3) #15](https://github.com/yanqirenshi/Spinor/issues/15)**
    - WSL2 環境で構築した LLVM JIT プロトタイプ (`spike/llvm-hs-investigation` ブランチ等) を拡張し、比較演算子（`=`, `<` 等）と条件分岐 (`if` 式) をコンパイルできるようにする。
- [ ] **[[Spike] LLVM JIT 拡張: ローカル変数 (`let`) と環境の導入 (Phase 4) #16](https://github.com/yanqirenshi/Spinor/issues/16)**
    - 比較演算と `if` 式を実装した LLVM JIT プロトタイプをさらに拡張し、ローカル変数の束縛 (`let`) と変数の参照をコンパイルできるようにする。
- [ ] **[[Spike] LLVM JIT 拡張: 関数定義 (`def` / `fn`) と関数呼び出し (Phase 5) #17](https://github.com/yanqirenshi/Spinor/issues/17)**
    - LLVM JIT プロトタイプの集大成として、ユーザー定義関数のコンパイルと呼び出しを実装する。
- [ ] **[[Spike] LLVM AOT 拡張: ネイティブ実行ファイルの生成 (Phase 6 完結編) #18](https://github.com/yanqirenshi/Spinor/issues/18)**
    - LLVM JIT コンパイラの技術検証の最終ステップとして、AOT (Ahead-Of-Time) コンパイルによるスタンドアロンな実行ファイルの生成機能を実装する。
- [ ] **[[Refactor] LLVM 依存ライブラリの削除と純粋 Haskell 化 (Windows ネイティブビルド対応) #19](https://github.com/yanqirenshi/Spinor/issues/19)**
    - Phase 6 (AOT コンパイル) において、LLVM IR をテキスト (`.ll`) として直接出力するアーキテクチャが完成した。
- [ ] **[[検証] Windows 11 (PowerShell) での AOT ネイティブコンパイルと master 統合 #20](https://github.com/yanqirenshi/Spinor/issues/20)**
    - 純粋な Haskell プロジェクトとしてリファクタリングされた LLVM AOT コンパイル機能を `master` ブランチへ統合する。
- [x] **[[Feature] HTTPサーバー基盤構築に向けた低水準 Socket API の実装 #7](https://github.com/yanqirenshi/Spinor/issues/7)**
    - Spinor 上で軽量な HTTP サーバーを構築するための第一歩として、ネットワーク通信の土台となる低水準な TCP Socket API を言語コアに実装する。
- [x] **[[App] はてなブックマーク CLI ツールの強化 (JSON API と `get-in` の活用) #14](https://github.com/yanqirenshi/Spinor/issues/14)**
    - Issue #10 で作成したはてなブックマーク CLI ツール (`app/hatena-b.spin`) を強化する。
- [x] **[[Docs] ビルドガイド (Build Guide) の更新: Windows ネイティブ対応と LLVM 依存の改訂 #21](https://github.com/yanqirenshi/Spinor/issues/21)**
    - LLVM AOT コンパイル機能の実装に伴い、Spinor のビルドプロセスが劇的に簡略化された。

## (3) ライブラリの充実 (Standard Library & Ecosystem)

言語が「実世界」とやり取りするためのインターフェースの構築。

- [ ] **[[Feature] HTTPサーバー高レベル API (`http-serve`) の実装 #8](https://github.com/yanqirenshi/Spinor/issues/8)**
    - Issue #7 で実装された低水準な Socket API (`socket-listen`, `socket-accept`, `socket-recv`, `socket-send`, `socket-close`) を活用し、Spinor (Lisp) 側で動作する軽量な HTTP サーバーフレームワークを構築する。
- [ ] **[[Feature] JSON パーサーのコア・プリミティブ実装 (`json-parse`) #12](https://github.com/yanqirenshi/Spinor/issues/12)**
    - Spinor 言語をモダンな Web API と連携させるため、JSON 文字列をパースして Spinor のネイティブデータ構造（Alist 等）に変換するコア・プリミティブ `(json-parse)` を実装する。
- [ ] **[[Feature] HTTPクライアントのレスポンスヘッダー取得サポート #5](https://github.com/yanqirenshi/Spinor/issues/5)**
    - 現在、`core-http-request` における HTTP レスポンスのヘッダー情報は未サポート（`(:headers nil)` を返却）となっている。
- [x] **[[Feature] HTTP/HTTPS クライアント機能の実装 (http-get, http-post) #2](https://github.com/yanqirenshi/Spinor/issues/2)**
    - Spinor 言語から外部 Web API と通信するための HTTP クライアント機能を実装する。
- [x] **[[Feature] HTTPサーバー基盤構築に向けた低水準 Socket API の実装 #7](https://github.com/yanqirenshi/Spinor/issues/7)**
    - Spinor 上で軽量な HTTP サーバーを構築するための第一歩として、ネットワーク通信の土台となる低水準な TCP Socket API を言語コアに実装する。
- [x] **[[App] はてなブックマーク CLI ツールの強化 (JSON API と `get-in` の活用) #14](https://github.com/yanqirenshi/Spinor/issues/14)**
    - Issue #10 で作成したはてなブックマーク CLI ツール (`app/hatena-b.spin`) を強化する。
- [x] **[[Docs] ビルドガイド (Build Guide) の更新: Windows ネイティブ対応と LLVM 依存の改訂 #21](https://github.com/yanqirenshi/Spinor/issues/21)**
    - LLVM AOT コンパイル機能の実装に伴い、Spinor のビルドプロセスが劇的に簡略化された。

## (4) アプリケーションの構築 (Real-world Applications)

エコシステムの検証とドッグフーディングを兼ねた、実用的なソフトウェアの構築。

- [ ] **[[App] はてなブックマーク CLI ツールの開発 (ドッグフーディング) #10](https://github.com/yanqirenshi/Spinor/issues/10)**
    - Spinor 言語の有用性を証明する「ドッグフーディング」の第一弾として、標準ライブラリの HTTP クライアント (`http-get`) を活用した実用的な CLI ツールを開発する。
- [x] **[[Feature] HTTPサーバー基盤構築に向けた低水準 Socket API の実装 #7](https://github.com/yanqirenshi/Spinor/issues/7)**
    - Spinor 上で軽量な HTTP サーバーを構築するための第一歩として、ネットワーク通信の土台となる低水準な TCP Socket API を言語コアに実装する。
- [x] **[[App] はてなブックマーク CLI ツールの強化 (JSON API と `get-in` の活用) #14](https://github.com/yanqirenshi/Spinor/issues/14)**
    - Issue #10 で作成したはてなブックマーク CLI ツール (`app/hatena-b.spin`) を強化する。
- [x] **[[Docs] ビルドガイド (Build Guide) の更新: Windows ネイティブ対応と LLVM 依存の改訂 #21](https://github.com/yanqirenshi/Spinor/issues/21)**
    - LLVM AOT コンパイル機能の実装に伴い、Spinor のビルドプロセスが劇的に簡略化された。

## (5) 利用促進 (Promotion & Developer Experience)

開発者が Spinor を発見し、迷わず使い始めるための環境整備。

- [ ] **[[Docs] ドキュメントディレクトリ構成の全面リニューアル (UX向上) #23](https://github.com/yanqirenshi/Spinor/issues/23)**
    - Spinor に実装された機能とドキュメント（全98ファイル）が増大してきたため、情報を探しやすい標準的な OSS のディレクトリ構成へと全面リニューアルする。
- [ ] **[[UI/UX] サイドバーの整理とランディングページの表示最適化 #24](https://github.com/yanqirenshi/Spinor/issues/24)**
    - ドキュメントサイトのナビゲーションUIをモダンな構成に最適化する。
- [ ] **[[Docs] Syntax ディレクトリの階層引き上げと CLHS 準拠のサイドバー整理 #25](https://github.com/yanqirenshi/Spinor/issues/25)**
    - 言語のコア仕様である「構文 (Syntax)」へのアクセス性を向上させるため、ドキュメントの URL パスを `docs/reference/syntax` から `docs/syntax` へ引き上げる。
- [ ] **[[UI/UX] レスポンシブ対応: スマホ表示時のサイドバー開閉機能（ハンバーガーメニュー）実装 #26](https://github.com/yanqirenshi/Spinor/issues/26)**
    - スマートフォンなどの画面幅が狭いデバイスで閲覧した際、固定サイドバーがメインコンテンツの表示領域を圧迫してしまう問題を解決する。
- [ ] **[[Docs] Installation ガイドのページ分割と階層化 #27](https://github.com/yanqirenshi/Spinor/issues/27)**
    - `installation.md` の情報量が増大し、1ページでの可読性が低下しているため、OSや目的ごとにファイルを分割する。
- [ ] **[GHCup の手順がわかりにくい #28](https://github.com/yanqirenshi/Spinor/issues/28)**
    - URL: https://yanqirenshi.github.io/Spinor/docs/installation/prerequisites
- [ ] **[MSYS2 は UCRT64 にしてほしい #29](https://github.com/yanqirenshi/Spinor/issues/29)**
    - url: https://yanqirenshi.github.io/Spinor/docs/installation/windows-msys2
- [ ] **[【警告修正】﻿EWithRegion/EAllocIn のパターンマッチ非網羅を修正する #30](https://github.com/yanqirenshi/Spinor/issues/30)**
    - リージョン機能で追加された `EWithRegion` と `EAllocIn` コンストラクタに対するパターンマッチが複数のモジュールで不足しており、`-Wincomplete-patterns` 警告が発生している。
- [ ] **[【警告修正】`VSocket` のパターンマッチ非網羅を修正する #31](https://github.com/yanqirenshi/Spinor/issues/31)**
    - `VSocket` コンストラクタに対するパターンマッチが `Server.hs` の複数関数で不足しており、`-Wincomplete-patterns` 警告が発生している。
- [ ] **[【警告修正】未使用インポート・未使用定義・名前シャドウイングの警告を解消する #32](https://github.com/yanqirenshi/Spinor/issues/32)**
    - ビルド時に `-Wunused-imports`、`-Wunused-matches`、`-Wunused-top-binds`、`-Wname-shadowing` の警告が複数発生している。
- [ ] **[ネイティブコンパイル (AOT) は instration とは別項にする。 #33](https://github.com/yanqirenshi/Spinor/issues/33)**
    - https://yanqirenshi.github.io/Spinor/docs/installation/aot
- [ ] **[共通の前提条件 では MSYS2 をインストールしていることにする。 #34](https://github.com/yanqirenshi/Spinor/issues/34)**
    - https://yanqirenshi.github.io/Spinor/docs/installation/prerequisites
- [ ] **[Windows 11 (PowerShell) の GHCup のインストール が失敗する。 #35](https://github.com/yanqirenshi/Spinor/issues/35)**
    - url: https://yanqirenshi.github.io/Spinor/docs/installation/prerequisites
- [ ] **[Windows 11 (PowerShell) の 最小構成でのビルド は不要 #36](https://github.com/yanqirenshi/Spinor/issues/36)**
    - url: https://yanqirenshi.github.io/Spinor/docs/installation/windows-powershell
- [ ] **[[Doc] マニュアルへの HTTP リクエスト (ネットワーク通信) セクションの追加 #6](https://github.com/yanqirenshi/Spinor/issues/6)**
    - Spinor が外部ネットワークと通信できるようになったことを受けて、ユーザーマニュアルに専用のセクションを追加する。
- [x] **[[Feature] HTTPサーバー基盤構築に向けた低水準 Socket API の実装 #7](https://github.com/yanqirenshi/Spinor/issues/7)**
    - Spinor 上で軽量な HTTP サーバーを構築するための第一歩として、ネットワーク通信の土台となる低水準な TCP Socket API を言語コアに実装する。
- [x] **[[App] はてなブックマーク CLI ツールの強化 (JSON API と `get-in` の活用) #14](https://github.com/yanqirenshi/Spinor/issues/14)**
    - Issue #10 で作成したはてなブックマーク CLI ツール (`app/hatena-b.spin`) を強化する。
- [x] **[[Docs] ビルドガイド (Build Guide) の更新: Windows ネイティブ対応と LLVM 依存の改訂 #21](https://github.com/yanqirenshi/Spinor/issues/21)**
    - LLVM AOT コンパイル機能の実装に伴い、Spinor のビルドプロセスが劇的に簡略化された。
- [x] **[[Docs] ビルドガイド (Build Guide) の更新: Windows ネイティブ対応と LLVM 依存の改訂 #22](https://github.com/yanqirenshi/Spinor/issues/22)**
    - LLVM AOT コンパイル機能の実装に伴い、Spinor のビルドプロセスが劇的に簡略化された。

## (6) テストの充実 (Testing & Quality Assurance)

言語コアやライブラリの信頼性を担保し、安全な開発サイクルを回すためのテスト基盤の強化。

- [x] **[[Feature] HTTPサーバー基盤構築に向けた低水準 Socket API の実装 #7](https://github.com/yanqirenshi/Spinor/issues/7)**
    - Spinor 上で軽量な HTTP サーバーを構築するための第一歩として、ネットワーク通信の土台となる低水準な TCP Socket API を言語コアに実装する。
- [x] **[[App] はてなブックマーク CLI ツールの強化 (JSON API と `get-in` の活用) #14](https://github.com/yanqirenshi/Spinor/issues/14)**
    - Issue #10 で作成したはてなブックマーク CLI ツール (`app/hatena-b.spin`) を強化する。
- [x] **[[Docs] ビルドガイド (Build Guide) の更新: Windows ネイティブ対応と LLVM 依存の改訂 #21](https://github.com/yanqirenshi/Spinor/issues/21)**
    - LLVM AOT コンパイル機能の実装に伴い、Spinor のビルドプロセスが劇的に簡略化された。

## (7) 究極の目標: Lispマシンの創生 (The Ultimate Dream)

Spinor を単なるアプリケーションレベルの処理系から、ハードウェアと直接対話する「Lispマシン (OS / ハードウェア)」へと昇華させる。

- [x] **[[Feature] HTTPサーバー基盤構築に向けた低水準 Socket API の実装 #7](https://github.com/yanqirenshi/Spinor/issues/7)**
    - Spinor 上で軽量な HTTP サーバーを構築するための第一歩として、ネットワーク通信の土台となる低水準な TCP Socket API を言語コアに実装する。
- [x] **[[App] はてなブックマーク CLI ツールの強化 (JSON API と `get-in` の活用) #14](https://github.com/yanqirenshi/Spinor/issues/14)**
    - Issue #10 で作成したはてなブックマーク CLI ツール (`app/hatena-b.spin`) を強化する。
- [x] **[[Docs] ビルドガイド (Build Guide) の更新: Windows ネイティブ対応と LLVM 依存の改訂 #21](https://github.com/yanqirenshi/Spinor/issues/21)**
    - LLVM AOT コンパイル機能の実装に伴い、Spinor のビルドプロセスが劇的に簡略化された。
