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
import Spinor.Syntax (Expr(..), SourceSpan)
import Spinor.BorrowCheck (BorrowResult(..))
import Spinor.EscapeAnalysis (EscapeResult(..))

-- | C コードの型エイリアス
type CCode = Text

-- | プログラム全体を C 言語のソースコードに変換する
--
-- defun 式は C のトップレベル関数に変換され、
-- その他のトップレベル式は main 関数内で評価・表示される。
compileProgram :: [Expr] -> CCode
compileProgram exprs =
    let (defuns, others) = partition isDefun exprs
        funDefs = T.unlines (map compileFunDef defuns)
        mainStmts = T.unlines (map compileStmt others)
    in T.unlines
        [ "#include <stdio.h>"
        , "#include <stdbool.h>"
        , "#include \"spinor.h\""
        , ""
        , glIncludes
        , glHelpers
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
    in T.unlines
        [ "#include <stdio.h>"
        , "#include <stdlib.h>  /* for free() - ownership system */"
        , "#include <stdbool.h>"
        , "#include \"spinor.h\""
        , ""
        , glIncludes
        , glHelpers
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
compileProgramWithRegions exprs escapeResult =
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
        , glIncludes
        , glHelpers
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
    , "            obj->value.str = str;"
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
compileStmt :: Expr -> CCode
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
    , "    _sp_gl_window = SDL_CreateWindow(title->value.str,"
    , "        SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,"
    , "        (int)w->value.integer, (int)h->value.integer, SDL_WINDOW_OPENGL);"
    , "    _sp_gl_context = SDL_GL_CreateContext(_sp_gl_window);"
    , "#else"
    , "    glfwInit();"
    , "    _sp_gl_window = glfwCreateWindow((int)w->value.integer,"
    , "        (int)h->value.integer, title->value.str, NULL, NULL);"
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
    , "    glBegin(GL_POINTS);"
    , "    /* TODO: extract vertex data from SpObject matrix */"
    , "    glEnd();"
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
