{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Spinor.Compiler.Codegen
Description : Spinor AST から C99 コードを生成するトランスパイラ

Spinor のサブセットを C 言語に変換する。生成されたコードは
runtime/spinor.h および runtime/spinor.c と組み合わせて使用する。
-}
module Spinor.Compiler.Codegen
  ( compileProgram
  , compileProgramWithOwnership
  , compileProgramWithRegions
  , compileExpr
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.List (partition)
import Data.Char (isAlphaNum)
import Control.Monad.State (State, get, put, modify, evalState)
import Spinor.Syntax (Expr(..), SourceSpan)
import Spinor.BorrowCheck (BorrowResult(..))
import Spinor.EscapeAnalysis (EscapeResult(..))

-- | C コードの型エイリアス
type CCode = Text

-- | AST 中に OpenGL 関連のシンボル (gl-*) が含まれるか検査する
usesGL :: [Expr] -> Bool
usesGL = any exprUsesGL
  where
    glSymbols :: [Text]
    glSymbols = ["gl-init", "gl-clear", "gl-draw-points",
                 "gl-swap-buffers", "gl-window-should-close"]
    exprUsesGL :: Expr -> Bool
    exprUsesGL (ESym _ s) = s `elem` glSymbols
    exprUsesGL (EList _ xs) = any exprUsesGL xs
    exprUsesGL (ELet _ binds body) =
      any (exprUsesGL . snd) binds || exprUsesGL body
    exprUsesGL (EWithRegion _ _ body) = exprUsesGL body
    exprUsesGL (EAllocIn _ _ body) = exprUsesGL body
    exprUsesGL _ = False

-- | プログラム全体を C 言語のソースコードに変換する
--
-- defun 式は C のトップレベル関数に変換され、
-- その他のトップレベル式は main 関数内で評価・表示される。
compileProgram :: [Expr] -> CCode
compileProgram exprs =
    let (defuns, others) = partition isDefun exprs
        funDefs = T.unlines (map compileFunDefAuto defuns)
        mainStmts = T.unlines (map compileStmtAuto others)
        glCode = if usesGL exprs
                 then T.unlines [glIncludes, glHelpers]
                 else ""
    in T.unlines
        [ "#include <stdio.h>"
        , "#include <stdbool.h>"
        , "#include \"spinor.h\""
        , ""
        , glCode
        , funDefs
        , "int main(void) {"
        , mainStmts
        , "    return 0;"
        , "}"
        ]

-- | 所有権情報を使ってプログラムをコンパイル (Experimental)
--
-- BorrowResult の dropPoints に基づき、適切な位置で free() を自動挿入する。
compileProgramWithOwnership :: [Expr] -> BorrowResult -> CCode
compileProgramWithOwnership exprs borrowResult =
    let (defuns, others) = partition isDefun exprs
        funDefs = T.unlines (map compileFunDef defuns)
        mainStmts = T.unlines (map compileStmt others)
        freeStmts = generateFreeStatements (brDropPoints borrowResult)
        glCode = if usesGL exprs
                 then T.unlines [glIncludes, glHelpers]
                 else ""
    in T.unlines
        [ "#include <stdio.h>"
        , "#include <stdlib.h>  /* for free() - ownership system */"
        , "#include <stdbool.h>"
        , "#include \"spinor.h\""
        , ""
        , glCode
        , funDefs
        , "int main(void) {"
        , mainStmts
        , freeStmts
        , "    return 0;"
        , "}"
        ]

-- | 所有権情報から free() 文を生成
generateFreeStatements :: Map.Map Text SourceSpan -> CCode
generateFreeStatements dropPoints =
    if Map.null dropPoints
    then ""
    else T.unlines $
        [ "    /* --- Automatic memory management (ownership system) --- */" ] ++
        [ "    sp_free(" <> mangle name <> ");  /* drop point */"
        | (name, _) <- Map.toList dropPoints
        ]

-- | リージョン情報を使ってプログラムをコンパイル (Experimental)
--
-- EscapeResult の検証に基づき、Arena アロケータのコードを生成する。
compileProgramWithRegions :: [Expr] -> EscapeResult -> CCode
compileProgramWithRegions exprs _escapeResult =
    let (defuns, others) = partition isDefun exprs
        funDefs = T.unlines (map compileFunDef defuns)
        mainStmts = T.unlines (map compileStmtWithRegion others)
    in T.unlines
        [ "#include <stdio.h>"
        , "#include <stdlib.h>"
        , "#include <stdbool.h>"
        , "#include <string.h>"
        , "#include \"spinor.h\""
        , ""
        , arenaAllocatorCode
        , ""
        , if usesGL exprs
          then T.unlines [glIncludes, glHelpers]
          else ""
        , funDefs
        , "int main(void) {"
        , mainStmts
        , "    return 0;"
        , "}"
        ]

-- | Arena アロケータの C ランタイムコード
arenaAllocatorCode :: CCode
arenaAllocatorCode = T.unlines
    [ "/* --- Region-based Memory Management (Arena Allocator) --- */"
    , ""
    , "#define REGION_DEFAULT_SIZE (64 * 1024)  /* 64 KB */"
    , ""
    , "typedef struct RegionBlock {"
    , "    struct RegionBlock* next;"
    , "    size_t size;"
    , "    size_t used;"
    , "    char data[];  /* Flexible array member */"
    , "} RegionBlock;"
    , ""
    , "typedef struct Region {"
    , "    RegionBlock* head;"
    , "    RegionBlock* current;"
    , "} Region;"
    , ""
    , "/* Create a new region (arena) */"
    , "Region* create_region(void) {"
    , "    Region* r = (Region*)malloc(sizeof(Region));"
    , "    if (!r) return NULL;"
    , "    RegionBlock* block = (RegionBlock*)malloc(sizeof(RegionBlock) + REGION_DEFAULT_SIZE);"
    , "    if (!block) { free(r); return NULL; }"
    , "    block->next = NULL;"
    , "    block->size = REGION_DEFAULT_SIZE;"
    , "    block->used = 0;"
    , "    r->head = block;"
    , "    r->current = block;"
    , "    return r;"
    , "}"
    , ""
    , "/* Allocate memory from a region */"
    , "void* region_alloc(Region* r, size_t size) {"
    , "    /* Align to 8 bytes */"
    , "    size = (size + 7) & ~7;"
    , "    RegionBlock* block = r->current;"
    , "    if (block->used + size > block->size) {"
    , "        /* Allocate new block */"
    , "        size_t newSize = (size > REGION_DEFAULT_SIZE) ? size : REGION_DEFAULT_SIZE;"
    , "        RegionBlock* newBlock = (RegionBlock*)malloc(sizeof(RegionBlock) + newSize);"
    , "        if (!newBlock) return NULL;"
    , "        newBlock->next = NULL;"
    , "        newBlock->size = newSize;"
    , "        newBlock->used = 0;"
    , "        block->next = newBlock;"
    , "        r->current = newBlock;"
    , "        block = newBlock;"
    , "    }"
    , "    void* ptr = block->data + block->used;"
    , "    block->used += size;"
    , "    return ptr;"
    , "}"
    , ""
    , "/* Destroy a region and free all memory */"
    , "void destroy_region(Region* r) {"
    , "    RegionBlock* block = r->head;"
    , "    while (block) {"
    , "        RegionBlock* next = block->next;"
    , "        free(block);"
    , "        block = next;"
    , "    }"
    , "    free(r);"
    , "}"
    , ""
    , "/* Allocate SpObject in a region */"
    , "SpObject* sp_region_alloc(Region* r) {"
    , "    SpObject* obj = (SpObject*)region_alloc(r, sizeof(SpObject));"
    , "    if (obj) {"
    , "        obj->type = SP_NIL;"
    , "    }"
    , "    return obj;"
    , "}"
    , ""
    , "/* Create integer in region */"
    , "SpObject* sp_region_make_int(Region* r, int64_t n) {"
    , "    SpObject* obj = sp_region_alloc(r);"
    , "    if (obj) {"
    , "        obj->type = SP_INT;"
    , "        obj->value.integer = n;"
    , "    }"
    , "    return obj;"
    , "}"
    , ""
    , "/* Create string in region */"
    , "SpObject* sp_region_make_str(Region* r, const char* s) {"
    , "    SpObject* obj = sp_region_alloc(r);"
    , "    if (obj) {"
    , "        size_t len = strlen(s) + 1;"
    , "        char* str = (char*)region_alloc(r, len);"
    , "        if (str) {"
    , "            memcpy(str, s, len);"
    , "            obj->type = SP_STR;"
    , "            obj->value.string = str;"
    , "        }"
    , "    }"
    , "    return obj;"
    , "}"
    ]

-- | with-region を含む式をステートメントに変換
compileStmtWithRegion :: Expr -> CCode
compileStmtWithRegion (EWithRegion _ regionName body) =
    T.unlines
        [ "    { /* with-region " <> regionName <> " */"
        , "        Region* " <> mangle regionName <> " = create_region();"
        , compileStmtWithRegion body
        , "        destroy_region(" <> mangle regionName <> ");"
        , "    }"
        ]
compileStmtWithRegion expr = compileStmt expr

-- | defun 式かどうかを判定する
isDefun :: Expr -> Bool
isDefun (EList _ (ESym _ "defun" : _)) = True
isDefun _ = False

-- | defun 式を C の関数定義に変換する
--
-- 末尾自己再帰が検出された場合は TCO (while(1) + continue) を適用する。
compileFunDef :: Expr -> CCode
compileFunDef (EList _ [ESym _ "defun", ESym _ name, EList _ argExprs, body]) =
    let cName = mangle name
        cArgs = T.intercalate ", " (map toCArg argExprs)
        paramNames = [n | ESym _ n <- argExprs]
    in if hasTailSelfCall name body
       then -- TCO 適用: while(1) ループで末尾再帰を最適化
            T.unlines
              [ "SpObject* " <> cName <> "(" <> cArgs <> ") {"
              , "    while(1) {"
              , compileTailBody name paramNames body
              , "    }"
              , "}"
              ]
       else -- TCO 非適用: 従来通り return
            T.unlines
              [ "SpObject* " <> cName <> "(" <> cArgs <> ") {"
              , "    return " <> compileExpr body <> ";"
              , "}"
              ]
  where
    toCArg (ESym _ argName) = "SpObject* " <> mangle argName
    toCArg _ = "SpObject* _unknown"
compileFunDef _ = "/* invalid defun */"

-- | 関数本体の末尾位置に自己再帰呼び出しがあるかを判定する
--
-- if 式の場合は両分岐を検査する。
hasTailSelfCall :: Text -> Expr -> Bool
hasTailSelfCall fname (EList _ [ESym _ "if", _, thenE, elseE]) =
    hasTailSelfCall fname thenE || hasTailSelfCall fname elseE
hasTailSelfCall fname (EList _ (ESym _ f : _)) = f == fname
hasTailSelfCall _ _ = False

-- | 末尾位置の式を TCO 対応の C コード (文) に変換する
--
-- - if 式: C の if/else 文に変換し、各分岐を再帰的に処理
-- - 自己再帰呼び出し: 一時変数で引数評価 → パラメータ更新 → continue
-- - その他: return 文を生成
compileTailBody :: Text -> [Text] -> Expr -> CCode
-- if 式: 分岐を C の if/else 文に変換
compileTailBody fname params (EList _ [ESym _ "if", cond, thenE, elseE]) =
    T.unlines
      [ "        if (" <> compileExpr cond <> "->value.boolean) {"
      , compileTailBody fname params thenE
      , "        } else {"
      , compileTailBody fname params elseE
      , "        }"
      ]
-- 末尾自己再帰呼び出し: 引数を一時変数に退避してから更新 + continue
compileTailBody fname params (EList _ (ESym _ f : args))
    | f == fname =
        let compiledArgs = map compileExpr args
            indexedArgs = zip [0::Int ..] compiledArgs
            indexedParams = zip [0::Int ..] params
            tmpDecls = T.unlines
              [ "            SpObject* _tco_tmp_" <> T.pack (show i) <> " = " <> a <> ";"
              | (i, a) <- indexedArgs ]
            assigns = T.unlines
              [ "            " <> mangle p <> " = _tco_tmp_" <> T.pack (show i) <> ";"
              | (i, p) <- indexedParams ]
        in tmpDecls <> assigns <> "            continue;"
-- その他の式: return で値を返す
compileTailBody _ _ expr =
    "            return " <> compileExpr expr <> ";"

-- | Spinor シンボルを安全な C 識別子に変換する (名前マングリング)
--
-- ユーザー定義関数には user_ プレフィックスを付与し、
-- C の識別子として無効な文字は _ に置換する。
mangle :: Text -> CCode
mangle name = "user_" <> T.map sanitize name
  where
    sanitize c | isAlphaNum c = c
               | otherwise    = '_'

-- | トップレベル式を C のステートメントに変換
--
-- 式を評価し、その結果を sp_print で表示する。
-- ただし `(print x)` のような副作用プリミティブが直接トップレベルにある場合は
-- 二重出力を避けるため、外側ラップを省略してプリミティブ呼び出しのみ生成する。
compileStmt :: Expr -> CCode
-- `(print x)` を top-level に書いた場合: 外側の sp_print 自動ラップをスキップ
-- (compileExpr が `sp_print(...)` を生成するため、二重出力を防ぐ)
compileStmt expr@(EList _ [ESym _ "print", _]) =
    "    " <> compileExpr expr <> ";"
-- `(drop x)` を top-level に書いた場合: 解放のみ行い、結果 (Unit) は表示しない
compileStmt (EDrop _ e) =
    "    sp_free(" <> compileExpr e <> ");"
compileStmt expr =
    let valCode = compileExpr expr
    in "    sp_print(" <> valCode <> ");"

-- | 式を C の式 (SpObject* を返すコード) に変換する
--
-- 各 Spinor 式は SpObject* を返す C の式に変換される。
compileExpr :: Expr -> CCode
-- リテラル
compileExpr (EInt _ n)  = "sp_make_int(" <> T.pack (show n) <> ")"
compileExpr (EBool _ b) = "sp_make_bool(" <> (if b then "true" else "false") <> ")"
compileExpr (EStr _ s)  = "sp_make_str(\"" <> escapeC s <> "\")"

-- if 式: (if cond then else)
compileExpr (EList _ [ESym _ "if", cond, thenE, elseE]) =
    "(" <> compileExpr cond <> "->value.boolean ? " <>
    compileExpr thenE <> " : " <> compileExpr elseE <> ")"

-- 算術演算
compileExpr (EList _ [ESym _ "+", a, b]) =
    "sp_add(" <> compileExpr a <> ", " <> compileExpr b <> ")"
compileExpr (EList _ [ESym _ "-", a, b]) =
    "sp_sub(" <> compileExpr a <> ", " <> compileExpr b <> ")"
compileExpr (EList _ [ESym _ "*", a, b]) =
    "sp_mul(" <> compileExpr a <> ", " <> compileExpr b <> ")"
compileExpr (EList _ [ESym _ "/", a, b]) =
    "sp_div(" <> compileExpr a <> ", " <> compileExpr b <> ")"

-- 比較演算
compileExpr (EList _ [ESym _ "=", a, b]) =
    "sp_eq(" <> compileExpr a <> ", " <> compileExpr b <> ")"
compileExpr (EList _ [ESym _ "<", a, b]) =
    "sp_lt(" <> compileExpr a <> ", " <> compileExpr b <> ")"
compileExpr (EList _ [ESym _ ">", a, b]) =
    "sp_gt(" <> compileExpr a <> ", " <> compileExpr b <> ")"
compileExpr (EList _ [ESym _ "<=", a, b]) =
    "sp_lte(" <> compileExpr a <> ", " <> compileExpr b <> ")"
compileExpr (EList _ [ESym _ ">=", a, b]) =
    "sp_gte(" <> compileExpr a <> ", " <> compileExpr b <> ")"

-- 文字列操作
compileExpr (EList _ [ESym _ "string-append", a, b]) =
    "sp_str_append(" <> compileExpr a <> ", " <> compileExpr b <> ")"
compileExpr (EList _ [ESym _ "string-length", s]) =
    "sp_str_length(" <> compileExpr s <> ")"
compileExpr (EList _ [ESym _ "substring", s, start, end]) =
    "sp_substring(" <> compileExpr s <> ", " <> compileExpr start <> ", " <> compileExpr end <> ")"
compileExpr (EList _ [ESym _ "string=?", a, b]) =
    "sp_str_eq(" <> compileExpr a <> ", " <> compileExpr b <> ")"

-- ファイル I/O
compileExpr (EList _ [ESym _ "read-file", path]) =
    "sp_read_file(" <> compileExpr path <> ")"
compileExpr (EList _ [ESym _ "write-file", path, content]) =
    "sp_write_file(" <> compileExpr path <> ", " <> compileExpr content <> ")"
compileExpr (EList _ [ESym _ "append-file", path, content]) =
    "sp_append_file(" <> compileExpr path <> ", " <> compileExpr content <> ")"
compileExpr (EList _ [ESym _ "file-exists?", path]) =
    "sp_file_exists(" <> compileExpr path <> ")"

-- 出力プリミティブ
-- `print` は runtime/spinor.h の sp_print に直接マッピングする (mangle 経由しない)。
-- sp_print は SpObject* を返す (Lisp 伝統: print は引数をそのまま返す) ので
-- 式コンテキストでも安全に使える。
compileExpr (EList _ [ESym _ "print", x]) =
    "sp_print(" <> compileExpr x <> ")"

-- 所有権の明示的破棄 (Phase R2-1 / Issue #76): (drop x) → sp_free
-- 式コンテキストでは GNU C の statement expression で nil を返す
-- (with-region と同じ手法。gcc/clang 前提)
compileExpr (EDrop _ e) =
    "({ sp_free(" <> compileExpr e <> "); sp_make_nil(); })"

-- リスト操作プリミティブ (Issue #75: ベアメタル AOT でのリスト実証用)
compileExpr (EList _ [ESym _ "cons", a, b]) =
    "sp_cons(" <> compileExpr a <> ", " <> compileExpr b <> ")"
compileExpr (EList _ [ESym _ "car", p]) =
    "sp_car(" <> compileExpr p <> ")"
compileExpr (EList _ [ESym _ "cdr", p]) =
    "sp_cdr(" <> compileExpr p <> ")"
compileExpr (EList _ [ESym _ "null?", x]) =
    "sp_is_nil(" <> compileExpr x <> ")"
-- (list a b ...) はネストした sp_cons に展開する
compileExpr (EList _ (ESym _ "list" : elems)) =
    foldr (\e acc -> "sp_cons(" <> compileExpr e <> ", " <> acc <> ")")
          "sp_make_nil()" elems

-- OpenGL プリミティブ
compileExpr (EList _ [ESym _ "gl-init", w, h, title]) =
    "sp_gl_init(" <> compileExpr w <> ", " <> compileExpr h <> ", " <> compileExpr title <> ")"
compileExpr (EList _ [ESym _ "gl-clear"]) =
    "sp_gl_clear()"
compileExpr (EList _ [ESym _ "gl-draw-points", m]) =
    "sp_gl_draw_points(" <> compileExpr m <> ")"
compileExpr (EList _ [ESym _ "gl-swap-buffers", win]) =
    "sp_gl_swap_buffers(" <> compileExpr win <> ")"
compileExpr (EList _ [ESym _ "gl-window-should-close", win]) =
    "sp_gl_window_should_close(" <> compileExpr win <> ")"

-- Experimental: Region-based memory management
-- with-region は式としては使用できないため、ブロック文として展開
compileExpr (EWithRegion _ regionName body) =
    "({ Region* " <> mangle regionName <> " = create_region(); " <>
    "SpObject* _region_result = " <> compileExpr body <> "; " <>
    "destroy_region(" <> mangle regionName <> "); _region_result; })"

-- alloc-in: リージョン内での割り当て
compileExpr (EAllocIn _ regionName expr) =
    case expr of
        EInt _ n -> "sp_region_make_int(" <> mangle regionName <> ", " <> T.pack (show n) <> ")"
        EStr _ s -> "sp_region_make_str(" <> mangle regionName <> ", \"" <> escapeC s <> "\")"
        _ -> compileExpr expr  -- フォールバック

-- 変数参照 (引数など)
compileExpr (ESym _ s) = mangle s

-- ユーザー定義関数の呼び出し (プリミティブでない関数)
compileExpr (EList _ (ESym _ fname : args))
    | fname `notElem` primitives =
        let cFun = mangle fname
            cArgs = T.intercalate ", " (map compileExpr args)
        in cFun <> "(" <> cArgs <> ")"
  where
    primitives = ["+", "-", "*", "/", "=", "<", ">", "<=", ">=", "if", "defun",
                  "print",
                  "cons", "car", "cdr", "list", "null?",
                  "string-append", "string-length", "substring", "string=?",
                  "read-file", "write-file", "append-file", "file-exists?",
                  "gl-init", "gl-clear", "gl-draw-points",
                  "gl-swap-buffers", "gl-window-should-close"]

-- 未実装のパターン
compileExpr other = "sp_make_nil() /* TODO: " <> T.pack (show other) <> " */"

-- ===========================================================================
-- OpenGL / WebGL (WASM) 対応
-- ===========================================================================

-- | #ifdef __EMSCRIPTEN__ による条件分岐インクルード
glIncludes :: CCode
glIncludes = T.unlines
    [ "#ifdef __EMSCRIPTEN__"
    , "#include <SDL2/SDL.h>"
    , "#include <GLES2/gl2.h>"
    , "#else"
    , "#include <GLFW/glfw3.h>"
    , "#include <GL/gl.h>"
    , "#endif"
    ]

-- | OpenGL ヘルパー関数 (ネイティブ/WASM 両対応)
glHelpers :: CCode
glHelpers = T.unlines
    [ "/* --- OpenGL helpers (native / WASM) --- */"
    , "#ifdef __EMSCRIPTEN__"
    , "static SDL_Window*   _sp_gl_window  = NULL;"
    , "static SDL_GLContext  _sp_gl_context = NULL;"
    , "#else"
    , "static GLFWwindow*   _sp_gl_window  = NULL;"
    , "#endif"
    , ""
    , "SpObject* sp_gl_init(SpObject* w, SpObject* h, SpObject* title) {"
    , "#ifdef __EMSCRIPTEN__"
    , "    SDL_Init(SDL_INIT_VIDEO);"
    , "    _sp_gl_window = SDL_CreateWindow(title->value.string,"
    , "        SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,"
    , "        (int)w->value.integer, (int)h->value.integer, SDL_WINDOW_OPENGL);"
    , "    _sp_gl_context = SDL_GL_CreateContext(_sp_gl_window);"
    , "#else"
    , "    glfwInit();"
    , "    _sp_gl_window = glfwCreateWindow((int)w->value.integer,"
    , "        (int)h->value.integer, title->value.string, NULL, NULL);"
    , "    glfwMakeContextCurrent(_sp_gl_window);"
    , "#endif"
    , "    return sp_make_bool(true);"
    , "}"
    , ""
    , "SpObject* sp_gl_clear(void) {"
    , "    glClear(GL_COLOR_BUFFER_BIT);"
    , "    return sp_make_nil();"
    , "}"
    , ""
    , "SpObject* sp_gl_swap_buffers(SpObject* win) {"
    , "    (void)win;"
    , "#ifdef __EMSCRIPTEN__"
    , "    SDL_GL_SwapWindow(_sp_gl_window);"
    , "#else"
    , "    glfwSwapBuffers(_sp_gl_window);"
    , "    glfwPollEvents();"
    , "#endif"
    , "    return sp_make_nil();"
    , "}"
    , ""
    , "SpObject* sp_gl_window_should_close(SpObject* win) {"
    , "    (void)win;"
    , "#ifdef __EMSCRIPTEN__"
    , "    SDL_Event event;"
    , "    while (SDL_PollEvent(&event)) {"
    , "        if (event.type == SDL_QUIT) return sp_make_bool(true);"
    , "    }"
    , "    return sp_make_bool(false);"
    , "#else"
    , "    return sp_make_bool(glfwWindowShouldClose(_sp_gl_window));"
    , "#endif"
    , "}"
    , ""
    , "SpObject* sp_gl_draw_points(SpObject* data) {"
    , "    (void)data;"
    , "    /* TODO: implement draw_points with shaders (glBegin/glEnd not available in ES 2.0) */"
    , "    return sp_make_nil();"
    , "}"
    ]

-- | C 文字列リテラルのエスケープ
escapeC :: Text -> Text
escapeC = T.concatMap escapeChar
  where
    escapeChar '"'  = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar '\t' = "\\t"
    escapeChar '\r' = "\\r"
    escapeChar c    = T.singleton c

-- ===========================================================================
-- 自動 Drop 挿入 (Phase R2-2 / Issue #77)
-- ===========================================================================
--
-- 式を ANF (A-normal form) 風に平坦化し、すべての中間値を C の一時変数
-- `SpObject* _tN` に束縛したうえで、コンパイル時の所有権クラスに基づいて
-- スコープ終端 (文末 / return 前 / TCO continue 前 / if 分岐内) に
-- sp_free を自動挿入する。
--
-- 所有権クラス:
--   * Fresh — 新規確保されたオブジェクト。このスコープが所有し、解放責任を持つ。
--             (sp_make_* / 算術・比較 / sp_is_nil / 文字列演算 / sp_cons の結果)
--   * Alias — 他者の構造への参照、または出所不明。解放してはならない (保守的)。
--             (sp_car / sp_cdr / sp_print の結果、ユーザー関数の結果、パラメータ)
--
-- moved フラグ: sp_cons への格納・TCO 再束縛・明示 (drop x) で所有権が移動した
-- Fresh temp は解放対象から外す (二重解放防止)。
--
-- TCO のパラメータ再束縛はランタイムフラグ `bool _owned_p` で管理し、
-- 「前イテレーションで Fresh 値に再束縛されていた場合のみ」旧値を解放する
-- (初回の呼出元借用値は解放しない)。

-- | 一時変数の所有権クラス
data Own = Fresh | Alias
    deriving (Eq, Show)

-- | 一時変数の記録
data Temp = Temp
    { tName  :: Text
    , tOwn   :: Own
    , tMoved :: Bool
    }

-- | ANF エミッタの状態
data GenSt = GenSt
    { gsCounter :: Int       -- ^ 一時変数 / ラベルの連番
    , gsStmts   :: [CCode]   -- ^ 発行済み C 文 (逆順)
    , gsTemps   :: [Temp]    -- ^ 現在のスコープで生成した一時変数
    }

type Gen = State GenSt

-- | インデント (関数本体は 4 スペース固定。ネスト if は C 的には動くが
--   可読性のための深いインデントはしない)
ind :: CCode
ind = "    "

-- | 文を発行する
emit :: CCode -> Gen ()
emit s = modify $ \st -> st { gsStmts = s : gsStmts st }

-- | 新しい一時変数に式を束縛する
bindTemp :: Own -> CCode -> Gen Text
bindTemp own initExpr = do
    st <- get
    let n    = gsCounter st
        name = "_t" <> T.pack (show n)
    put st { gsCounter = n + 1
           , gsStmts   = (ind <> "SpObject* " <> name <> " = " <> initExpr <> ";")
                         : gsStmts st
           , gsTemps   = Temp name own False : gsTemps st
           }
    pure name

-- | int 一時変数 (cond の bool 退避用) を確保する
bindIntTemp :: CCode -> Gen Text
bindIntTemp initExpr = do
    st <- get
    let n    = gsCounter st
        name = "_c" <> T.pack (show n)
    put st { gsCounter = n + 1
           , gsStmts   = (ind <> "int " <> name <> " = " <> initExpr <> ";")
                         : gsStmts st
           }
    pure name

-- | 名前が一時変数レジストリにあれば moved 扱いにする
--   (パラメータ等、レジストリ外の名前は何もしない)
markMovedT :: Text -> Gen ()
markMovedT name = modify $ \st ->
    st { gsTemps = map upd (gsTemps st) }
  where
    upd t | tName t == name = t { tMoved = True }
          | otherwise       = t

-- | サブスコープを実行し、(発行された文, 結果) を返す。
--   counter は共有し、temps / stmts はサブスコープ独立。
subScope :: Gen a -> Gen ([CCode], [Temp], a)
subScope action = do
    st <- get
    let saved = st
    put st { gsStmts = [], gsTemps = [] }
    result <- action
    st' <- get
    put saved { gsCounter = gsCounter st' }
    pure (reverse (gsStmts st'), gsTemps st', result)

-- | スコープの解放コードを生成する: Fresh かつ未 moved の temp を解放し、
--   Alias temp には未使用警告避けの (void) を発行する。
--   except に挙げた名前 (返り値等) は対象外。
scopeFrees :: [Text] -> [Temp] -> [CCode]
scopeFrees except temps = concatMap freeOne (reverse temps)
  where
    freeOne t
        | tName t `elem` except = []
        | tMoved t              = []
        | tOwn t == Fresh       = [ind <> "sp_free(" <> tName t <> ");"]
        | otherwise             = [ind <> "(void)" <> tName t <> ";"]

-- | 現在のスコープの temps を取得
getTemps :: Gen [Temp]
getTemps = fmap gsTemps get

-- ---------------------------------------------------------------------------
-- 式の ANF コンパイル
-- ---------------------------------------------------------------------------

-- | 2 引数プリミティブ (引数は借用、結果は Fresh)
freshPrims2 :: [(Text, Text)]
freshPrims2 =
    [ ("+", "sp_add"), ("-", "sp_sub"), ("*", "sp_mul"), ("/", "sp_div")
    , ("=", "sp_eq"), ("<", "sp_lt"), (">", "sp_gt")
    , ("<=", "sp_lte"), (">=", "sp_gte")
    , ("string-append", "sp_str_append"), ("string=?", "sp_str_eq")
    ]

-- | 1 引数プリミティブ (引数は借用、結果は Fresh)
freshPrims1 :: [(Text, Text)]
freshPrims1 = [ ("null?", "sp_is_nil"), ("string-length", "sp_str_length") ]

-- | 1 引数プリミティブ (引数は借用、結果は Alias = 解放禁止)
aliasPrims1 :: [(Text, Text)]
aliasPrims1 = [ ("car", "sp_car"), ("cdr", "sp_cdr"), ("print", "sp_print") ]

-- | ANF 用プリミティブ名の集合 (ユーザー関数呼出と区別する)
anfPrimitives :: [Text]
anfPrimitives =
    map fst freshPrims2 ++ map fst freshPrims1 ++ map fst aliasPrims1
    ++ ["if", "defun", "cons", "list", "substring", "quote"]

-- | 式を ANF で発行し、(結果の C 名, 所有権クラス) を返す
compileExprA :: Expr -> Gen (Text, Own)

-- リテラル → Fresh temp
compileExprA (EInt _ n)  = do
    name <- bindTemp Fresh ("sp_make_int(" <> T.pack (show n) <> ")")
    pure (name, Fresh)
compileExprA (EBool _ b) = do
    name <- bindTemp Fresh ("sp_make_bool(" <> (if b then "true" else "false") <> ")")
    pure (name, Fresh)
compileExprA (EStr _ s)  = do
    name <- bindTemp Fresh ("sp_make_str(\"" <> escapeC s <> "\")")
    pure (name, Fresh)

-- 変数参照 (パラメータ等): temp を作らず名前をそのまま使う。Alias。
compileExprA (ESym _ s) = pure (mangle s, Alias)

-- 空リスト
compileExprA (EList _ []) = do
    name <- bindTemp Fresh "sp_make_nil()"
    pure (name, Fresh)

-- if 式 (非末尾): 文の if に展開し、結果を共有 temp に代入する。
--   cond の bool は int に退避してから cond temp を解放できるようにする
--   (解放自体はスコープ終端で行う)。
compileExprA (EList _ [ESym _ "if", c, t, e]) = do
    (cName, _) <- compileExprA c
    condVal <- bindIntTemp (cName <> "->value.boolean")
    -- 結果受け取り用の temp (両分岐で代入)
    st <- get
    let n     = gsCounter st
        rName = "_t" <> T.pack (show n)
    put st { gsCounter = n + 1 }
    emit (ind <> "SpObject* " <> rName <> " = NULL;")
    -- then 分岐 (サブスコープ: 分岐内 temp は分岐内で解放)
    (thenStmts, thenTemps, (tn, tOwnC)) <- subScope (compileExprA t)
    let thenFrees = scopeFrees [tn] thenTemps
    -- else 分岐
    (elseStmts, elseTemps, (en, eOwnC)) <- subScope (compileExprA e)
    let elseFrees = scopeFrees [en] elseTemps
    emit (ind <> "if (" <> condVal <> ") {")
    mapM_ emit thenStmts
    emit (ind <> rName <> " = " <> tn <> ";")
    mapM_ emit thenFrees
    emit (ind <> "} else {")
    mapM_ emit elseStmts
    emit (ind <> rName <> " = " <> en <> ";")
    mapM_ emit elseFrees
    emit (ind <> "}")
    -- 結果の所有権: 両分岐 Fresh のときのみ Fresh (混在は保守的に Alias)
    let rOwn = if tOwnC == Fresh && eOwnC == Fresh then Fresh else Alias
    modify $ \s -> s { gsTemps = Temp rName rOwn False : gsTemps s }
    pure (rName, rOwn)

-- quote: 定数リスト (リテラルのみ想定) — 保守的に素通しの nil
compileExprA (EList _ [ESym _ "quote", _]) = do
    name <- bindTemp Fresh "sp_make_nil()"
    pure (name, Fresh)

-- cons: 引数の所有権はセルに移動する (moved)
compileExprA (EList _ [ESym _ "cons", a, b]) = do
    (an, _) <- compileExprA a
    (bn, _) <- compileExprA b
    markMovedT an
    markMovedT bn
    name <- bindTemp Fresh ("sp_cons(" <> an <> ", " <> bn <> ")")
    pure (name, Fresh)

-- list: ネストした cons に展開。要素と内側セルの所有権は外側セルへ移動。
compileExprA (EList _ (ESym _ "list" : elems)) = do
    elemNames <- mapM (fmap fst . compileExprA) elems
    nilName <- bindTemp Fresh "sp_make_nil()"
    let build acc en = do
            markMovedT en
            markMovedT acc
            bindTemp Fresh ("sp_cons(" <> en <> ", " <> acc <> ")")
    root <- foldM' build nilName (reverse elemNames)
    pure (root, Fresh)
  where
    foldM' _ z []     = pure z
    foldM' f z (x:xs) = f z x >>= \z' -> foldM' f z' xs

-- substring (3 引数, 引数借用, 結果 Fresh)
compileExprA (EList _ [ESym _ "substring", s, st_, en_]) = do
    (sn, _) <- compileExprA s
    (stn, _) <- compileExprA st_
    (enn, _) <- compileExprA en_
    name <- bindTemp Fresh
        ("sp_substring(" <> sn <> ", " <> stn <> ", " <> enn <> ")")
    pure (name, Fresh)

-- 明示的 drop: 対象を解放して moved 化し、nil を返す (Phase R2-1 互換)
compileExprA (EDrop _ e) = do
    (n, _) <- compileExprA e
    emit (ind <> "sp_free(" <> n <> ");")
    markMovedT n
    name <- bindTemp Fresh "sp_make_nil()"
    pure (name, Fresh)

-- 2 引数 Fresh プリミティブ
compileExprA (EList _ [ESym _ op, a, b])
    | Just cFun <- lookup op freshPrims2 = do
        (an, _) <- compileExprA a
        (bn, _) <- compileExprA b
        name <- bindTemp Fresh (cFun <> "(" <> an <> ", " <> bn <> ")")
        pure (name, Fresh)

-- 1 引数プリミティブ (Fresh / Alias)
compileExprA (EList _ [ESym _ op, x])
    | Just cFun <- lookup op freshPrims1 = do
        (xn, _) <- compileExprA x
        name <- bindTemp Fresh (cFun <> "(" <> xn <> ")")
        pure (name, Fresh)
    | Just cFun <- lookup op aliasPrims1 = do
        (xn, _) <- compileExprA x
        name <- bindTemp Alias (cFun <> "(" <> xn <> ")")
        pure (name, Alias)

-- ユーザー関数呼出: 引数は借用 (呼出元が解放)、結果は保守的に Alias。
compileExprA (EList _ (ESym _ f : args))
    | f `notElem` anfPrimitives = do
        argNames <- mapM (fmap fst . compileExprA) args
        name <- bindTemp Alias
            (mangle f <> "(" <> T.intercalate ", " argNames <> ")")
        pure (name, Alias)

-- 未対応パターン: 従来コンパイラにフォールバック (解放なし = Alias)
compileExprA other = do
    name <- bindTemp Alias (compileExpr other)
    pure (name, Alias)

-- ---------------------------------------------------------------------------
-- トップレベル文 / 関数定義
-- ---------------------------------------------------------------------------

-- | トップレベル式を main 内の C ブロックに変換する (自動解放付き)。
--   従来仕様と同じく、print / drop 以外の式は結果を sp_print で表示する。
compileStmtAuto :: Expr -> CCode
compileStmtAuto expr = evalState go (GenSt 0 [] [])
  where
    isPrint (EList _ [ESym _ "print", _]) = True
    isPrint _                             = False
    isDrop (EDrop _ _) = True
    isDrop _           = False
    go = do
        (rName, _) <- compileExprA expr
        if isPrint expr || isDrop expr
            then emit (ind <> "(void)" <> rName <> ";")
            else emit (ind <> "sp_print(" <> rName <> ");")
        temps <- getTemps
        st <- get
        let body  = reverse (gsStmts st)
            frees = scopeFrees [] temps
        pure $ T.unlines (["    {"] ++ map ("    " <>) (body ++ frees) ++ ["    }"])

-- | defun を C 関数に変換する (自動解放付き)。
--   末尾自己再帰があれば TCO (while ループ + パラメータ再束縛) を適用する。
compileFunDefAuto :: Expr -> CCode
compileFunDefAuto (EList _ [ESym _ "defun", ESym _ name, EList _ argExprs, body]) =
    let cName      = mangle name
        paramNames = [n | ESym _ n <- argExprs]
        cArgs      = if null paramNames
                     then "void"
                     else T.intercalate ", "
                            (map (\p -> "SpObject* " <> mangle p) paramNames)
        header     = "SpObject* " <> cName <> "(" <> cArgs <> ") {"
    in if hasTailSelfCall name body
       then
         let ownedDecls =
               [ ind <> "bool _owned_" <> mangle p <> " = false;"
               | p <- paramNames ]
             loopBody = evalState (compileTailA name paramNames body)
                                  (GenSt 0 [] [])
         in T.unlines
              ( [header]
                ++ ownedDecls
                ++ [ind <> "while (1) {"]
                ++ loopBody
                ++ [ind <> "}", "}"] )
       else
         let stmts = evalState goPlain (GenSt 0 [] [])
         in T.unlines ([header] ++ stmts ++ ["}"])
  where
    goPlain = do
        (rName, _) <- compileExprA body
        temps <- getTemps
        st <- get
        pure $ reverse (gsStmts st)
               ++ scopeFrees [rName] temps
               ++ [ind <> "return " <> rName <> ";"]
compileFunDefAuto _ = "/* invalid defun */"

-- | TCO 対象の末尾位置式を C 文列に変換する (自動解放付き)。
--   * if: cond を ANF 評価 → bool を int に退避 → cond の temp を解放 →
--         分岐それぞれを再帰処理 (分岐は独立スコープ)
--   * 自己再帰呼出: 引数を ANF 評価 → 旧パラメータ値を _owned_ フラグ付きで
--         解放 → 再束縛 → 残り temp を解放 → continue
--   * その他: ANF 評価 → 返り値以外を解放 → パラメータ旧値も解放 → return
compileTailA :: Text -> [Text] -> Expr -> Gen [CCode]
compileTailA fname params (EList _ [ESym _ "if", c, t, e]) = do
    (condStmts, condTemps, (cName, _)) <- subScope $ do
        (cn, _) <- compileExprA c
        cv <- bindIntTemp (cn <> "->value.boolean")
        pure (cv, ())
    -- cond で生成した temp は分岐前にすべて解放できる (bool は退避済み)
    let condFrees = scopeFrees [] condTemps
    thenStmts <- compileTailA fname params t
    elseStmts <- compileTailA fname params e
    pure $ condStmts ++ condFrees
           ++ [ind <> "if (" <> cName <> ") {"]
           ++ thenStmts
           ++ [ind <> "} else {"]
           ++ elseStmts
           ++ [ind <> "}"]
compileTailA fname params (EList _ (ESym _ f : args))
    | f == fname = do
        (stmts, temps, argResults) <- subScope (mapM compileExprA args)
        let argNames = map fst argResults
            argOwns  = map snd argResults
            -- 再束縛に渡す temp は moved (このスコープでは解放しない)
            temps'   = map (\tp -> if tName tp `elem` argNames
                                   then tp { tMoved = True } else tp) temps
            rebinds  = concat
                [ [ ind <> "if (_owned_" <> mangle p <> ") sp_free("
                        <> mangle p <> ");"
                  , ind <> mangle p <> " = " <> an <> ";"
                  , ind <> "_owned_" <> mangle p <> " = "
                        <> (if ow == Fresh then "true" else "false") <> ";"
                  ]
                | (p, (an, ow)) <- zip params (zip argNames argOwns) ]
            frees    = scopeFrees [] temps'
        pure $ stmts ++ rebinds ++ frees ++ [ind <> "continue;"]
compileTailA _ params expr = do
    (stmts, temps, (rName, rOwn)) <- subScope (compileExprA expr)
    let frees      = scopeFrees [rName] temps
        -- return 前: ループ内で再束縛された旧パラメータ値も解放する。
        -- ただし返り値がパラメータ由来の Alias の可能性があるため、
        -- 返り値が Fresh (パラメータと無関係) のときに限る。
        paramFrees
          | rOwn == Fresh =
              [ ind <> "if (_owned_" <> mangle p <> ") sp_free("
                    <> mangle p <> ");"
              | p <- params ]
          | otherwise = []
    pure $ stmts ++ frees ++ paramFrees ++ [ind <> "return " <> rName <> ";"]
