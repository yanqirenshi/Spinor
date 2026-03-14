# Usage Guide

Spinor の使い方に関するガイドです。

## 目次

- [Editor Setup](editor) - Emacs / SLY の設定方法
- [Cookbook](cookbook) - レシピ集・実践ガイド
- [AI Workflow](ai_workflow) - AI エージェントとの連携

## クイックスタート

```lisp
;; REPL を起動
;; $ cabal run spinor

;; 基本的な計算
spinor> (+ 1 2)
:: Int
3

;; 関数定義
spinor> (def square (fn (x) (* x x)))
:: Int -> Int

spinor> (square 5)
:: Int
25
```
