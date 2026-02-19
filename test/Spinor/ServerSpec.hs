{-# LANGUAGE OverloadedStrings #-}

module Spinor.ServerSpec (spec) where

import Test.Hspec

import Spinor.Syntax (Expr(..))
import Spinor.Server
    ( normalizeCommand
    , normalizeForm
    , extractTraceSpec
    , exprToText
    , mkOkResponse
    , mkAbortResponse
    , newTracedFunctions
    , addTracedFunction
    , removeTracedFunction
    , isTraced
    , getTracedFunctions
    , clearAllTraces
    )

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
            normalizeForm (EList [ESym "slynk:autodoc", EStr "test"])
                `shouldBe` EList [ESym "swank:autodoc", EStr "test"]

        it "leaves non-list expressions unchanged" $
            normalizeForm (EInt 42) `shouldBe` EInt 42

        it "normalizes nested trace-dialog command" $
            normalizeForm (EList [ESym "slynk-trace-dialog:dialog-toggle-trace", EStr "square"])
                `shouldBe` EList [ESym "swank:trace:dialog-toggle-trace", EStr "square"]

    describe "extractTraceSpec" $ do
        it "extracts from plain string" $
            extractTraceSpec (EStr "square") `shouldBe` "square"

        it "extracts from symbol" $
            extractTraceSpec (ESym "square") `shouldBe` "square"

        it "extracts from (slynk::from-string \"name\")" $
            extractTraceSpec (EList [ESym "slynk::from-string", EStr "square"])
                `shouldBe` "square"

        it "extracts from (swank::from-string \"name\")" $
            extractTraceSpec (EList [ESym "swank::from-string", EStr "my-func"])
                `shouldBe` "my-func"

        it "returns empty for unrecognized format" $
            extractTraceSpec (EInt 42) `shouldBe` ""

    describe "exprToText" $ do
        it "converts integer" $
            exprToText (EInt 42) `shouldBe` "42"

        it "converts true boolean" $
            exprToText (EBool True) `shouldBe` "t"

        it "converts false boolean" $
            exprToText (EBool False) `shouldBe` "nil"

        it "converts symbol" $
            exprToText (ESym ":ok") `shouldBe` ":ok"

        it "converts string with escaping" $
            exprToText (EStr "hello\"world") `shouldBe` "\"hello\\\"world\""

        it "converts empty list to nil" $
            exprToText (EList []) `shouldBe` "nil"

        it "converts non-empty list" $
            exprToText (EList [ESym ":return", EInt 1])
                `shouldBe` "(:return 1)"

        it "converts nested list" $
            exprToText (EList [ESym ":ok", EList [EInt 1, EInt 2]])
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
                exprToText (mkOkResponse 1 (EInt 42))
                    `shouldBe` "(:return (:ok 42) 1)"

            it "works with list result" $
                exprToText (mkOkResponse 5 (EList [EStr "hello"]))
                    `shouldBe` "(:return (:ok (\"hello\")) 5)"

            it "works with empty list (nil)" $
                exprToText (mkOkResponse 10 (EList []))
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
            let response = mkOkResponse 1 (EList
                    [ ESym ":pid", EInt 0
                    , ESym ":style", ESym ":spawn"
                    ])
            exprToText response `shouldBe`
                "(:return (:ok (:pid 0 :style :spawn)) 1)"

        it "mrepl create response format (channel-id thread-id)" $ do
            let response = mkOkResponse 1 (EList [EInt 1, EInt 0])
            exprToText response `shouldBe` "(:return (:ok (1 0)) 1)"

        it "trace toggle response format" $ do
            let response = mkOkResponse 1 (EStr "square is now traced for trace dialog")
            exprToText response `shouldBe`
                "(:return (:ok \"square is now traced for trace dialog\") 1)"
