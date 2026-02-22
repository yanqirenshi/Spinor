{-# LANGUAGE OverloadedStrings #-}

module Spinor.DocGen
  ( generateDocs
  ) where

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import Language.LSP.Protocol.Types (CompletionItemKind(..))

import Spinor.Lsp.Docs (DocEntry(..), allDocEntries)

-- | ドキュメント生成のエントリポイント
generateDocs :: IO ()
generateDocs = do
  putStrLn "Generating documentation..."

  -- docs/ref/ ディレクトリを作成
  createDirectoryIfMissing True "docs/ref"

  -- 各エントリの個別ファイルを生成
  let entries = allDocEntries
  mapM_ generateEntryFile entries

  -- インデックスファイルを生成
  generateIndexFile entries

  putStrLn $ "Generated " ++ show (length entries) ++ " reference files."
  putStrLn "Documentation generated successfully."

-- | 個別ファイルを生成
generateEntryFile :: (Text, DocEntry) -> IO ()
generateEntryFile (name, entry) = do
  let slug = docSlug entry
      path = "docs/ref/" ++ T.unpack slug ++ ".md"
      content = renderEntry name entry
  TIO.writeFile path content

-- | 個別エントリを CLHS スタイルの Markdown に変換
renderEntry :: Text -> DocEntry -> Text
renderEntry name entry = T.intercalate "\n" $ filter (not . T.null) $
  [ "# " <> name
  , ""
  , "**Kind:** " <> kindToText (docKind entry)
  , ""
  , "### Syntax:"
  , ""
  , "```lisp"
  , docSyntax entry
  , "```"
  , ""
  , "### Arguments and Values:"
  , ""
  , T.strip (docArgumentsAndValues entry)
  , ""
  , "### Description:"
  , ""
  , docDescription entry
  , ""
  , "### Examples:"
  , ""
  , T.strip (docExamples entry)
  , ""
  , "### Side Effects:"
  , ""
  , docSideEffects entry
  , ""
  , "### Affected By:"
  , ""
  , docAffectedBy entry
  , ""
  , "### Exceptional Situations:"
  , ""
  , docExceptionalSituations entry
  , ""
  ]
  ++ renderSeeAlso (docSeeAlso entry)
  ++ renderNotes (docNotes entry)

-- | See Also セクションをレンダリング
renderSeeAlso :: [Text] -> [Text]
renderSeeAlso [] = []
renderSeeAlso slugs =
  [ "### See Also:"
  , ""
  , T.intercalate ", " (map renderSlugLink slugs)
  , ""
  ]
  where
    renderSlugLink slug = "[" <> slug <> "](" <> slug <> ")"

-- | Notes セクションをレンダリング
renderNotes :: Text -> [Text]
renderNotes notes
  | T.null (T.strip notes) = []
  | otherwise =
      [ "### Notes:"
      , ""
      , T.strip notes
      , ""
      ]

-- | CompletionItemKind を表示用テキストに変換
kindToText :: CompletionItemKind -> Text
kindToText CompletionItemKind_Function = "Function"
kindToText CompletionItemKind_Keyword  = "Special Form"
kindToText _                           = "Other"

-- | インデックスファイルを生成
generateIndexFile :: [(Text, DocEntry)] -> IO ()
generateIndexFile entries = do
  let content = renderIndex entries
  TIO.writeFile "docs/reference.md" content

-- | インデックスを Markdown に変換
renderIndex :: [(Text, DocEntry)] -> Text
renderIndex entries = T.unlines $
  [ "# Spinor Reference"
  , ""
  , "Spinor の組み込み関数・特殊形式のリファレンスです。"
  , ""
  ]
  ++ renderCategory "Special Forms" specialForms
  ++ renderCategory "Arithmetic" arithmetic
  ++ renderCategory "Comparison" comparison
  ++ renderCategory "List Operations" listOps
  ++ renderCategory "String Operations" stringOps
  ++ renderCategory "I/O" ioOps
  ++ renderCategory "Concurrency" concurrency
  where
    -- カテゴリ分類
    specialForms = filter (isKeyword . snd) sorted
    arithmetic   = filter ((`elem` ["+", "-", "*", "%"]) . fst) sorted
    comparison   = filter ((`elem` ["=", "<", ">"]) . fst) sorted
    listOps      = filter (isListOp . fst) sorted
    stringOps    = filter (isStringOp . fst) sorted
    ioOps        = filter (isIOOp . fst) sorted
    concurrency  = filter (isConcurrencyOp . fst) sorted

    sorted = sortBy (comparing fst) entries

    isKeyword entry = docKind entry == CompletionItemKind_Keyword

    isListOp name = name `elem` ["cons", "car", "cdr", "list", "null?", "empty?", "eq", "equal"]
    isStringOp name = "string" `T.isPrefixOf` name || name `elem` ["list->string", "substring"]
    isIOOp name = name `elem` ["read-file", "write-file", "append-file", "file-exists?", "print"]
    isConcurrencyOp name = name `elem` ["spawn", "sleep", "new-mvar", "take-mvar", "put-mvar"]

-- | カテゴリ内のエントリをリスト化
renderCategory :: Text -> [(Text, DocEntry)] -> [Text]
renderCategory _ [] = []
renderCategory title entries =
  [ "## " <> title
  , ""
  ]
  ++ map renderLink entries
  ++ [""]
  where
    renderLink (name, entry) =
      "- [" <> name <> "](doc.html?src=ref/" <> docSlug entry <> ".md)"
