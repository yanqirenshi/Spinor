# トラブルシューティング

## DLL Not Found (Windows)

**症状:** `cabal test` や `cabal run` 実行時に以下のようなエラーが発生:
```
exit code: -1073741515 (0xC0000135)
```
または
```
libopenblas.dll が見つかりません
```

**原因:** Windows の cabal は PATH 環境変数を子プロセスに正しく渡さないことがあります。

**解決策:** 必要な DLL を実行ファイルと同じディレクトリにコピーします。

```powershell
# テスト実行ディレクトリ (GHC バージョンに合わせて調整)
$testDir = "dist-newstyle\build\x86_64-windows\ghc-9.6.7\spinor-0.1.0.0\t\spinor-test\build\spinor-test"

# DLL をコピー
Copy-Item C:\msys64\mingw64\bin\libopenblas.dll $testDir
Copy-Item C:\msys64\mingw64\bin\libgfortran-5.dll $testDir
Copy-Item C:\msys64\mingw64\bin\libquadmath-0.dll $testDir
Copy-Item C:\msys64\mingw64\bin\libgcc_s_seh-1.dll $testDir
Copy-Item C:\msys64\mingw64\bin\libwinpthread-1.dll $testDir
Copy-Item C:\msys64\mingw64\bin\OpenCL.dll $testDir
```

**注意:** `cabal clean` を実行すると DLL も削除されるため、再度コピーが必要です。

---

## OpenCL Platform Not Found

**症状:** `(cl-init)` を実行すると "No OpenCL platform found" エラー。

**原因:** OpenCL ICD (Installable Client Driver) が正しくインストールされていません。

**解決策:**

Linux:
```bash
# ICD Loader のインストール
sudo apt-get install -y ocl-icd-libopencl1

# 利用可能なプラットフォームの確認
clinfo
```

Windows:
```powershell
# MSYS2 で OpenCL ICD をインストール
pacman -S --noconfirm mingw-w64-x86_64-opencl-icd

# GPU ドライバの OpenCL サポートを確認
# - NVIDIA: CUDA Toolkit に含まれる
# - AMD: AMD Software に含まれる
# - Intel: Intel OpenCL Runtime
```

---

## hmatrix ビルドエラー (openblas not found)

**症状:**
```
Setup: Missing dependency on a foreign library: * Missing (or bad) C library: openblas
```

**解決策:**

1. OpenBLAS がインストールされているか確認:
   - Linux: `dpkg -l | grep openblas`
   - Windows: `pacman -Qs openblas`

2. `cabal.project.local` で正しいパスが設定されているか確認

3. Windows の場合、`flags: +openblas` が設定されているか確認

---

## pkg-config が見つからない

**症状:**
```
pkg-config: command not found
```

**解決策:**

Linux:
```bash
sudo apt-get install -y pkg-config
```

Windows (MSYS2):
```bash
pacman -S --noconfirm mingw-w64-x86_64-pkg-config
```

また、`C:\msys64\mingw64\bin` が PATH に含まれているか確認してください。
