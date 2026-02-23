# 引き継ぎファイル: Task 44 Matrix Foundation

## 現状
Task 44 (Matrix/Tensor Type Foundation) の実装が完了し、commit & push 済み。

## 確認が必要な項目
Windows 環境で `network` ライブラリのビルドエラー (`HsNetworkConfig.h` 不在) が発生し、`cabal test` を実行できなかった。WSL2 環境でビルドとテストの確認が必要。

## 実行コマンド
```bash
cd ~/prj/Spinor  # または適切なパス
git pull
cabal build
cabal test
```

## 期待される結果
- ビルド成功
- 全テストパス (特に「行列操作 (Matrix)」セクションの 9 テスト)

## 追加した行列テスト (test/Spinor/EvalSpec.hs)
1. `matrix で 2x2 行列を生成`
2. `mdim で行列の次元を取得`
3. `mref で要素を取得 (0,0)`
4. `mref で要素を取得 (1,1)`
5. `mref で要素を取得 (0,2)`
6. `matrix で要素数不一致はエラー`
7. `mref で範囲外アクセスはエラー (行)`
8. `mref で範囲外アクセスはエラー (列)`
9. `matrix は VInt を VFloat に変換`

## 変更ファイル一覧
- `spinor.cabal` - vector 依存追加
- `src/Spinor/Val.hs` - VFloat, VMatrix 型追加
- `src/Spinor/Primitive.hs` - matrix, mdim, mref 実装
- `src/Spinor/Lsp/Docs.hs` - CLHS ドキュメント追加
- `test/Spinor/EvalSpec.hs` - テストケース追加

## 問題発生時
テスト失敗やビルドエラーがあれば、エラーメッセージを確認して修正してください。
