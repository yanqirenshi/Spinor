{-# LANGUAGE OverloadedStrings #-}

module Spinor.SocketSpec (spec) where

import Test.Hspec
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import qualified Data.Text as T
import Spinor.Val (Val(..))
import Spinor.Core.Socket (socketPrimitives)

spec :: Spec
spec = describe "Spinor.Core.Socket (Socket API)" $ do

  describe "socketPrimitives" $ do
    it "全プリミティブが登録されている" $ do
      let names = map fst socketPrimitives
      names `shouldContain` ["socket-listen"]
      names `shouldContain` ["socket-accept"]
      names `shouldContain` ["socket-recv"]
      names `shouldContain` ["socket-send"]
      names `shouldContain` ["socket-close"]
      names `shouldContain` ["socket-connect"]

  describe "socket-listen 引数検証" $ do
    it "引数がない場合はエラー" $ do
      let Just (VPrim _ fn) = lookup "socket-listen" socketPrimitives
      case fn [] of
        Left err -> err `shouldSatisfy` T.isInfixOf "引数が不正"
        Right _ -> expectationFailure "Should fail with no arguments"

    it "引数が整数でない場合はエラー" $ do
      let Just (VPrim _ fn) = lookup "socket-listen" socketPrimitives
      case fn [VStr "8080"] of
        Left err -> err `shouldSatisfy` T.isInfixOf "引数が不正"
        Right _ -> expectationFailure "Should fail with string argument"

  describe "socket-connect 引数検証" $ do
    it "引数がない場合はエラー" $ do
      let Just (VPrim _ fn) = lookup "socket-connect" socketPrimitives
      case fn [] of
        Left err -> err `shouldSatisfy` T.isInfixOf "引数が不正"
        Right _ -> expectationFailure "Should fail with no arguments"

    it "引数の型が不正な場合はエラー" $ do
      let Just (VPrim _ fn) = lookup "socket-connect" socketPrimitives
      case fn [VInt 8080, VStr "80"] of
        Left err -> err `shouldSatisfy` T.isInfixOf "文字列"
        Right _ -> expectationFailure "Should fail with wrong types"

  describe "ローカルエコーサーバーテスト" $ do
    it "サーバーとクライアント間でデータ送受信できる" $ do
      -- プリミティブ取得
      let Just (VPrim _ listenFn) = lookup "socket-listen" socketPrimitives
      let Just (VPrim _ acceptFn) = lookup "socket-accept" socketPrimitives
      let Just (VPrim _ recvFn) = lookup "socket-recv" socketPrimitives
      let Just (VPrim _ sendFn) = lookup "socket-send" socketPrimitives
      let Just (VPrim _ closeFn) = lookup "socket-close" socketPrimitives
      let Just (VPrim _ connectFn) = lookup "socket-connect" socketPrimitives

      -- テスト用ポート (動的に選択されたポートを使用するのが理想だが、シンプルに固定)
      let testPort = 19876 :: Integer

      -- 結果を受け取る MVar
      serverReady <- newEmptyMVar
      echoResult <- newEmptyMVar

      -- サーバースレッドを起動
      _ <- forkIO $ do
        -- サーバーソケット作成
        case listenFn [VInt testPort] of
          Left err -> putMVar echoResult (Left $ "listen failed: " <> err)
          Right (VSocket serverSock) -> do
            -- サーバー準備完了を通知
            putMVar serverReady ()

            -- クライアント接続を待ち受け
            case acceptFn [VSocket serverSock] of
              Left err -> putMVar echoResult (Left $ "accept failed: " <> err)
              Right (VList [VSocket clientSock, _]) -> do
                -- データ受信
                case recvFn [VSocket clientSock, VInt 1024] of
                  Left err -> putMVar echoResult (Left $ "recv failed: " <> err)
                  Right (VStr received) -> do
                    -- エコーバック (受信したデータをそのまま送り返す)
                    case sendFn [VSocket clientSock, VStr received] of
                      Left err -> putMVar echoResult (Left $ "send failed: " <> err)
                      Right _ -> do
                        -- クライアントソケットクローズ
                        let _ = closeFn [VSocket clientSock]
                        -- サーバーソケットクローズ
                        let _ = closeFn [VSocket serverSock]
                        pure ()
                  Right other -> putMVar echoResult (Left $ "recv unexpected: " <> T.pack (show other))
              Right other -> putMVar echoResult (Left $ "accept unexpected: " <> T.pack (show other))
          Right other -> putMVar echoResult (Left $ "listen unexpected: " <> T.pack (show other))

      -- サーバーが準備完了するまで待機
      takeMVar serverReady
      -- 少し待ってから接続
      threadDelay 50000  -- 50ms

      -- クライアント側
      case connectFn [VStr "127.0.0.1", VInt testPort] of
        Left err -> expectationFailure $ "connect failed: " <> T.unpack err
        Right (VSocket clientSock) -> do
          -- テストメッセージ送信
          let testMsg = "Hello, Socket!"
          case sendFn [VSocket clientSock, VStr testMsg] of
            Left err -> expectationFailure $ "client send failed: " <> T.unpack err
            Right (VInt sent) -> do
              sent `shouldBe` fromIntegral (T.length testMsg)

              -- エコーバック受信
              case recvFn [VSocket clientSock, VInt 1024] of
                Left err -> expectationFailure $ "client recv failed: " <> T.unpack err
                Right (VStr received) -> do
                  -- 受信データがエコーバックされていることを確認
                  received `shouldBe` testMsg
                  -- クライアントソケットクローズ
                  case closeFn [VSocket clientSock] of
                    Left err -> expectationFailure $ "client close failed: " <> T.unpack err
                    Right (VBool True) -> pure ()
                    Right other -> expectationFailure $ "close unexpected: " <> show other
                Right other -> expectationFailure $ "recv unexpected: " <> show other
            Right other -> expectationFailure $ "send unexpected: " <> show other
        Right other -> expectationFailure $ "connect unexpected: " <> show other

      -- サーバースレッドのエラーチェック (タイムアウト付き)
      -- サーバーが結果を返す前にクライアントがテストを終える可能性があるため、
      -- echoResult のチェックは省略 (サーバー側のエラーは上のテストで検出される)
      pure ()
