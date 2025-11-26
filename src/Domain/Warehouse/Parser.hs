{-# LANGUAGE OverloadedStrings #-}

module Domain.Warehouse.Parser (parseIngestRequest, renderValidationErrors, toEither) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Char as C
import Text.Read (readMaybe)
import Data.Validation (Validation(..), toEither)
import Text.Regex.TDFA ((=~)) -- For article validation

-- Assuming these types are defined in Domain.Warehouse.Types
import Domain.Warehouse.Types
import Utils.Telegram.Markdown (escapeMarkdownV2)

--------------------------------------------------------------------------------
-- TEMPLATES (For Error Messages - Updated)
--------------------------------------------------------------------------------

rollTemplate :: Text
rollTemplate =
    "📄 **Standard Roll Format**\n" <>
    "`Name (Line 1)`\n" <>
    "`Длина рулона: 50 м (Line 2)`\n" <>
    "`Цена: 1500 руб/метр (Line 3)`\n" <>
    "`ART-123 (Line 4)`\n" <>
    "`Description...`"

preCutTemplate :: Text
preCutTemplate =
    "✂️ **Pre-Cut Format (must include #отрез)**\n" <>
    "`Name (Line 1)`\n" <>
    "`Длина: 1.2 м (Line 2)`\n" <>
    "`Цена: 2400 руб (Line 3)`\n" <>
    "`ART-123 (Line 4)`\n" <>
    "`Description...`\n" <>
    "`#отрез`"

--------------------------------------------------------------------------------
-- MAIN LOGIC
--------------------------------------------------------------------------------

-- | The entry point. The result is a Validation containing either a list of errors or the successful parse.
parseIngestRequest :: Text -> Validation [AdminParseError] Fabric
parseIngestRequest rawText =
  let
     cleanLines = 
       filter (not . T.null) $ 
         map T.strip $ T.lines rawText
     isPreCut = 
        "#отрез" `T.isInfixOf` T.toLower rawText || 
        "#лоскут" `T.isInfixOf` T.toLower rawText
  in
    if isPreCut
    then validatePreCut cleanLines
    else validateRoll cleanLines

--------------------------------------------------------------------------------
-- VALIDATORS (Applicative Style)
--------------------------------------------------------------------------------

-- | Validator for the Roll pattern (Name, Length, Price, Article)
validateRoll :: [Text] -> Validation [AdminParseError] Fabric
validateRoll lines
  | length lines < 4 = Failure [StructureError Roll "Need at least 4 lines for a Roll"]
  | otherwise =
      Fabric
        <$> pure (lines !! 0)                      -- Name
        <*> validatePrice Roll (lines !! 2)        -- Price (Line 3)
        <*> validateArticle (lines !! 3)           -- Article (Line 4)
        <*> pure (T.unlines (drop 4 lines))        -- Description
        <*> pure Roll
        <*> validateLength Roll (lines !! 1)       -- Length (Line 2)

-- | Validator for the Pre-Cut pattern (Name, Length, Price, Article)
validatePreCut :: [Text] -> Validation [AdminParseError] Fabric
validatePreCut lines
  | length lines < 4 = Failure [StructureError PreCut "Need at least 4 lines for a Pre-Cut"]
  | otherwise =
      Fabric
        <$> pure (lines !! 0)                         -- Name
        <*> validatePrice PreCut (lines !! 2)         -- Price (Line 3)
        <*> validateArticle (lines !! 3)              -- Article (Line 4)
        <*> pure (T.unlines (drop 4 lines))           -- Description
        <*> pure PreCut
        <*> validateLength PreCut (lines !! 1)        -- Length (Line 2)

--------------------------------------------------------------------------------
-- FIELD-LEVEL VALIDATORS & HELPERS
--------------------------------------------------------------------------------

-- | Validates an article string. Must contain only uppercase letters, numbers, and dashes.
validateArticle :: Text -> Validation [AdminParseError] Text
validateArticle articleRaw =
    let pattern = "^[A-Z0-9-]+$" :: String
    in if T.unpack articleRaw =~ pattern
        then Success articleRaw
        else Failure [InvalidArticleFormat articleRaw]

-- | Validates a price string, ensuring it starts with "Цена:".
validatePrice :: FabricType -> Text -> Validation [AdminParseError] Int
validatePrice fType raw =
  if "Цена" `T.isPrefixOf` raw
  then case extractInt raw of
         Just p  -> Success p
         Nothing -> Failure [ValueError fType ("Could not parse number from price line: " <> raw)]
  else Failure [ValueError fType ("Price line must start with 'Цена:'. Got: " <> raw)]

-- | Validates a length string, ensuring it starts with "Длина:".
validateLength :: FabricType -> Text -> Validation [AdminParseError] Double
validateLength fType raw =
  if "Длина" `T.isPrefixOf` raw
 then case extractDouble raw of
        Just l  -> Success l
        Nothing -> Failure [ValueError fType ("Could not parse number from length line: " <> raw)]
  else Failure [ValueError fType ("Length line must start with 'Длина:'. Got: " <> raw)]

-- | Helper to extract an Int from a string like "Цена: 1 500 руб".
extractInt :: Text -> Maybe Int
extractInt t = readMaybe (T.unpack $ T.filter C.isDigit t)

-- | Helper to extract a Double from a string like "Длина: 1,2 м".
extractDouble :: Text -> Maybe Double
extractDouble t =
  let norm = T.replace "," "." t
      clean = T.filter (\c -> C.isDigit c || c == '.') norm
  in readMaybe (T.unpack clean)

--------------------------------------------------------------------------------
-- ERROR RENDERING (The part that generates the nice message for the Bot)
--------------------------------------------------------------------------------

-- | Takes a list of errors and formats them into a single, user-friendly message.
renderValidationErrors :: [AdminParseError] -> Text
renderValidationErrors errors =
  let
     isPreCutError = any isPreCutContext errors
     isPreCutContext (StructureError PreCut _) = True
     isPreCutContext (ValueError PreCut _)     = True
     isPreCutContext _                         = False

     header = "❌ **Parsing Errors Found:**\n\n"
     errorText = T.intercalate "\n" $ map ("• " <>) $ map simpleErrorText errors
     template = if isPreCutError then preCutTemplate else rollTemplate
     hint = if not isPreCutError
            then "\n\n_PS: If this was a pre-cut, please add the `#отрез` tag._"
            else mempty
  in escapeMarkdownV2 $ header <> errorText <> "\n\n👇 **Expected Format:**\n" <> template <> hint

-- | Renders a single error from the list into a simple string.
simpleErrorText :: AdminParseError -> Text
simpleErrorText (StructureError _ msg) = msg
simpleErrorText (ValueError _ msg) = msg
simpleErrorText (AmbiguousFormat msg) = msg
simpleErrorText (InvalidArticleFormat t) = "Invalid Article format: `" <> t <> "`. Use only A-Z, 0-9, and dashes (-)."