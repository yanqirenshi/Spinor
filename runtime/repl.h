/**
 * Spinor REPL Evaluation Header
 *
 * ブラウザ REPL 用の評価関数を宣言する。
 * sp_eval_string() は Emscripten 経由で JS から呼び出される。
 */

#ifndef SPINOR_REPL_H
#define SPINOR_REPL_H

#include "spinor.h"

/**
 * 文字列を受け取り、S 式として評価し、結果を文字列で返す。
 * 返り値は static バッファへのポインタ（呼び出し元で free しない）。
 */
const char* sp_eval_string(const char* input);

/**
 * SpObject を文字列にフォーマットする。
 * 返り値は static バッファへのポインタ。
 */
const char* sp_format(SpObject* obj);

#endif /* SPINOR_REPL_H */
