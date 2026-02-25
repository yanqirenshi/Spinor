{-# LANGUAGE OverloadedStrings #-}

module Spinor.ServerSpec (spec) where

import Test.Hspec
import qualified Data.Text as T

import Spinor.Syntax (Expr(..), dummySpan)
import Spinor.Server

-- | 短縮版の Expr コンストラクタ (テスト用)
eInt :: Integer -> Expr
eInt = EInt dummySpan

eBool :: Bool -> Expr
eBool = EBool dummySpan

eStr :: T.Text -> Expr
eStr = EStr dummySpan

eSym :: T.Text -> Expr
eSym = ESym dummySpan

eList :: [Expr] -> Expr
eList = EList dummySpan

spec :: Spec
spec = do
    describe "normalizeCommand" $ do
        describe "slynk: prefix" $ do
            it "slynk:connection-info -> swank:connection-info" $
                normalizeCommand "slynk:connection-info" `shouldBe` "swank:connection-info"
            it "slynk:autodoc -> swank:autodoc" $
                normalizeCommand "slynk:autodoc" `shouldBe` "swank:autodoc"

        describe "slynk-completion: prefix" $ do
            it "slynk-completion:flex-completions -> swank:completion:flex-completions" $
                normalizeCommand "slynk-completion:flex-completions" `shouldBe` "swank:completion:flex-completions"

        describe "slynk-mrepl: prefix" $ do
            it "slynk-mrepl:create-mrepl -> swank:mrepl:create-mrepl" $
                normalizeCommand "slynk-mrepl:create-mrepl" `shouldBe` "swank:mrepl:create-mrepl"

        describe "slynk-trace-dialog: prefix" $ do
            it "slynk-trace-dialog:dialog-toggle-trace -> swank:trace:dialog-toggle-trace" $
                normalizeCommand "slynk-trace-dialog:dialog-toggle-trace" `shouldBe` "swank:trace:dialog-toggle-trace"
            it "slynk-trace-dialog:report-specs -> swank:trace:report-specs" $
                normalizeCommand "slynk-trace-dialog:report-specs" `shouldBe` "swank:trace:report-specs"

        describe "slynk-stickers: prefix" $ do
            it "slynk-stickers:fetch -> swank:stickers:fetch" $
                normalizeCommand "slynk-stickers:fetch" `shouldBe` "swank:stickers:fetch"
            it "slynk-stickers:total-recordings -> swank:stickers:total-recordings" $
                normalizeCommand "slynk-stickers:total-recordings" `shouldBe` "swank:stickers:total-recordings"

        describe "slynk-profiler: prefix" $ do
            it "slynk-profiler:toggle-timing -> swank:profiler:toggle-timing" $
                normalizeCommand "slynk-profiler:toggle-timing" `shouldBe` "swank:profiler:toggle-timing"
            it "slynk-profiler:report-latest-timings -> swank:profiler:report-latest-timings" $
                normalizeCommand "slynk-profiler:report-latest-timings" `shouldBe` "swank:profiler:report-latest-timings"

        describe "slynk-package-fu: prefix" $ do
            it "slynk-package-fu:list-all-package-names -> swank:package-fu:list-all-package-names" $
                normalizeCommand "slynk-package-fu:list-all-package-names" `shouldBe` "swank:package-fu:list-all-package-names"
            it "slynk-package-fu:set-package -> swank:package-fu:set-package" $
                normalizeCommand "slynk-package-fu:set-package" `shouldBe` "swank:package-fu:set-package"
            it "slynk-package-fu:apropos-package -> swank:package-fu:apropos-package" $
                normalizeCommand "slynk-package-fu:apropos-package" `shouldBe` "swank:package-fu:apropos-package"

        describe "slynk-macrostep: prefix" $ do
            it "slynk-macrostep:macrostep-expand-1 -> swank:macrostep:macrostep-expand-1" $
                normalizeCommand "slynk-macrostep:macrostep-expand-1" `shouldBe` "swank:macrostep:macrostep-expand-1"
            it "slynk-macrostep:macrostep-expand -> swank:macrostep:macrostep-expand" $
                normalizeCommand "slynk-macrostep:macrostep-expand" `shouldBe` "swank:macrostep:macrostep-expand"
            it "slynk-macrostep:compiler-macroexpand-1 -> swank:macrostep:compiler-macroexpand-1" $
                normalizeCommand "slynk-macrostep:compiler-macroexpand-1" `shouldBe` "swank:macrostep:compiler-macroexpand-1"

        describe "slynk-apropos: prefix" $ do
            it "slynk-apropos:apropos-list-for-emacs -> swank:apropos:apropos-list-for-emacs" $
                normalizeCommand "slynk-apropos:apropos-list-for-emacs" `shouldBe` "swank:apropos:apropos-list-for-emacs"

        describe "slynk-xref: prefix" $ do
            it "slynk-xref:xref -> swank:xref:xref" $
                normalizeCommand "slynk-xref:xref" `shouldBe` "swank:xref:xref"
            it "slynk-xref:xrefs -> swank:xref:xrefs" $
                normalizeCommand "slynk-xref:xrefs" `shouldBe` "swank:xref:xrefs"

        describe "slynk:slynk- prefix" $ do
            it "slynk:slynk-add-load-paths -> swank:swank-add-load-paths" $
                normalizeCommand "slynk:slynk-add-load-paths" `shouldBe` "swank:swank-add-load-paths"

        describe "passthrough" $ do
            it "already swank: prefixed commands pass through" $
                normalizeCommand "swank:connection-info" `shouldBe` "swank:connection-info"
            it "unknown prefixes pass through" $
                normalizeCommand "other:command" `shouldBe` "other:command"

    describe "normalizeForm" $ do
        it "normalizes command symbol in list" $
            normalizeForm (eList [eSym "slynk:autodoc", eStr "test"])
                `shouldBe` eList [eSym "swank:autodoc", eStr "test"]

        it "leaves non-list expressions unchanged" $
            normalizeForm (eInt 42) `shouldBe` eInt 42

        it "normalizes nested trace-dialog command" $
            normalizeForm (eList [eSym "slynk-trace-dialog:dialog-toggle-trace", eStr "square"])
                `shouldBe` eList [eSym "swank:trace:dialog-toggle-trace", eStr "square"]

    describe "extractTraceSpec" $ do
        it "extracts from plain string" $
            extractTraceSpec (eStr "square") `shouldBe` "square"

        it "extracts from symbol" $
            extractTraceSpec (eSym "square") `shouldBe` "square"

        it "extracts from (slynk::from-string \"name\")" $
            extractTraceSpec (eList [eSym "slynk::from-string", eStr "square"])
                `shouldBe` "square"

        it "extracts from (swank::from-string \"name\")" $
            extractTraceSpec (eList [eSym "swank::from-string", eStr "my-func"])
                `shouldBe` "my-func"

        it "returns empty for unrecognized format" $
            extractTraceSpec (eInt 42) `shouldBe` ""

    describe "exprToText" $ do
        it "converts integer" $
            exprToText (eInt 42) `shouldBe` "42"

        it "converts true boolean" $
            exprToText (eBool True) `shouldBe` "t"

        it "converts false boolean" $
            exprToText (eBool False) `shouldBe` "nil"

        it "converts symbol" $
            exprToText (eSym ":ok") `shouldBe` ":ok"

        it "converts string with escaping" $
            exprToText (eStr "hello\"world") `shouldBe` "\"hello\\\"world\""

        it "converts empty list to nil" $
            exprToText (eList []) `shouldBe` "nil"

        it "converts non-empty list" $
            exprToText (eList [eSym ":return", eInt 1])
                `shouldBe` "(:return 1)"

        it "converts nested list" $
            exprToText (eList [eSym ":ok", eList [eInt 1, eInt 2]])
                `shouldBe` "(:ok (1 2))"

    describe "TracedFunctions state management" $ do
        it "new state is empty" $ do
            tf <- newTracedFunctions
            funcs <- getTracedFunctions tf
            funcs `shouldBe` []

        it "addTracedFunction adds a function" $ do
            tf <- newTracedFunctions
            addTracedFunction tf "square"
            traced <- isTraced tf "square"
            traced `shouldBe` True

        it "isTraced returns False for non-traced function" $ do
            tf <- newTracedFunctions
            traced <- isTraced tf "unknown"
            traced `shouldBe` False

        it "removeTracedFunction removes a function" $ do
            tf <- newTracedFunctions
            addTracedFunction tf "square"
            removeTracedFunction tf "square"
            traced <- isTraced tf "square"
            traced `shouldBe` False

        it "getTracedFunctions returns all traced functions" $ do
            tf <- newTracedFunctions
            addTracedFunction tf "square"
            addTracedFunction tf "cube"
            addTracedFunction tf "double"
            funcs <- getTracedFunctions tf
            length funcs `shouldBe` 3

        it "clearAllTraces clears all traced functions" $ do
            tf <- newTracedFunctions
            addTracedFunction tf "square"
            addTracedFunction tf "cube"
            clearAllTraces tf
            funcs <- getTracedFunctions tf
            funcs `shouldBe` []

        it "adding same function twice is idempotent" $ do
            tf <- newTracedFunctions
            addTracedFunction tf "square"
            addTracedFunction tf "square"
            funcs <- getTracedFunctions tf
            length funcs `shouldBe` 1

    describe "Response builders" $ do
        describe "mkOkResponse" $ do
            it "creates (:return (:ok result) reqId) format" $
                exprToText (mkOkResponse 1 (eInt 42))
                    `shouldBe` "(:return (:ok 42) 1)"

            it "works with list result" $
                exprToText (mkOkResponse 5 (eList [eStr "hello"]))
                    `shouldBe` "(:return (:ok (\"hello\")) 5)"

            it "works with empty list (nil)" $
                exprToText (mkOkResponse 10 (eList []))
                    `shouldBe` "(:return (:ok nil) 10)"

        describe "mkAbortResponse" $ do
            it "creates (:return (:abort message) reqId) format" $
                exprToText (mkAbortResponse 1 "Error occurred")
                    `shouldBe` "(:return (:abort \"Error occurred\") 1)"

            it "escapes quotes in error message" $
                exprToText (mkAbortResponse 2 "Unknown \"symbol\"")
                    `shouldBe` "(:return (:abort \"Unknown \\\"symbol\\\"\") 2)"

    describe "RPC Protocol" $ do
        it "connection-info response format" $ do
            -- Test that connection info response has required fields
            let response = mkOkResponse 1 (eList
                    [ eSym ":pid", eInt 0
                    , eSym ":style", eSym ":spawn"
                    ])
            exprToText response `shouldBe`
                "(:return (:ok (:pid 0 :style :spawn)) 1)"

        it "mrepl create response format (channel-id thread-id)" $ do
            let response = mkOkResponse 1 (eList [eInt 1, eInt 0])
            exprToText response `shouldBe` "(:return (:ok (1 0)) 1)"

        it "trace toggle response format" $ do
            let response = mkOkResponse 1 (eStr "square is now traced for trace dialog")
            exprToText response `shouldBe`
                "(:return (:ok \"square is now traced for trace dialog\") 1)"
