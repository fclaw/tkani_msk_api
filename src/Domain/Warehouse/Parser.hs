{-# LANGUAGE OverloadedStrings #-}

module Domain.Warehouse.Parser (parseIngestRequest, renderValidationErrors, toEither, validateLength) where

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
    "`Ширина: 140 см (Line 3)`\n" <>
    "`Цена: 1500 руб/метр (Line 4)`\n" <>
    "`ART-123 (Line 5)`\n" <>
    "`Description...`" <>
    "`#_search|_nosearch`"

preCutTemplate :: Text
preCutTemplate =
    "✂️ **Pre-Cut Format (#отрез)**\n" <>
    "`Name (Line 1)`\n" <>
    "`Длина: 1.2 м (Line 2)`\n" <>
    "`Ширина: 140 см (Line 3)`\n" <>
    "`Цена: 2400 руб (Line 4)`\n" <>
    "`ART-123 (Line 5)`\n" <>
    "`Description...`\n" <>
    "`#отрез`" <>
    "`#_search|_nosearch`"

--------------------------------------------------------------------------------
-- MAIN LOGIC
--------------------------------------------------------------------------------

-- | The entry point. The result is a Validation containing either a list of errors or the successful parse.
parseIngestRequest :: Text -> Double -> Validation [ParseError] Fabric
parseIngestRequest rawText threshold =
    let
        allLines = map T.strip $ T.lines rawText
        lowerText = T.toLower rawText

        -- 1. Detect and Validate Visibility Tags
        hasSearchTag = "#_search" `T.isInfixOf` lowerText
        hasNoSearchTag = "#_nosearch" `T.isInfixOf` lowerText

    in
    if not hasSearchTag && not hasNoSearchTag then
        Failure [MissingVisibilityTag]
    else if hasSearchTag && hasNoSearchTag then
        Failure [AmbiguousFormat "Cannot have both #_search and #_nosearch tags."]
    else
        let
            isSearchable = hasSearchTag

            -- 2. === THE FIX: Filter out the meta tags ===
            --    Create a new list of lines that does not contain our visibility tags.
            contentLines = filter (not . isVisibilityTag) allLines
            
            -- Now, filter for empty lines from this new list
            cleanLines = filter (not . T.null) contentLines

            isPreCut = 
              "#отрез" `T.isInfixOf` lowerText || 
              "#лоскут" `T.isInfixOf` lowerText
        in
        -- 3. Pass the CLEANED lines to the validators
        if isPreCut
            then validatePreCut cleanLines isSearchable
            else validateRoll cleanLines threshold isSearchable

-- | Helper function to identify visibility tag lines.
isVisibilityTag :: Text -> Bool
isVisibilityTag line =
  let lowerLine = T.toLower line
  in T.isInfixOf "#_search" lowerLine || 
     T.isInfixOf "#_nosearch" lowerLine

--------------------------------------------------------------------------------
-- VALIDATORS (Applicative Style)
--------------------------------------------------------------------------------

-- | Validator for the Roll pattern (Name, Length, Price, Article)
validateRoll :: [Text] -> Double -> Bool -> Validation [ParseError] Fabric
validateRoll lines threshold isSearchable
  | length lines < 4 = Failure [StructureError Roll "Need at least 4 lines for a Roll"]
  | otherwise =
      Fabric
        <$> pure (lines !! 0)                      -- Name
        <*> validatePrice Roll (lines !! 3)        -- Price (Line 3)
        <*> validateArticle (lines !! 4)           -- Article (Line 4)
        <*> pure (T.unlines (drop 5 lines))        -- Description
        <*> pure Roll
        <*> validateLength Roll (lines !! 1) (Just threshold) -- Length (Line 2)
        <*> validateWidth Roll (lines !! 2)
        <*> pure isSearchable

-- | Validator for the Pre-Cut pattern (Name, Length, Price, Article)
validatePreCut :: [Text] -> Bool -> Validation [ParseError] Fabric
validatePreCut lines isSearchable
  | length lines < 4 = Failure [StructureError PreCut "Need at least 4 lines for a Pre-Cut"]
  | otherwise =
      Fabric
        <$> pure (lines !! 0)                         -- Name
        <*> validatePrice PreCut (lines !! 3)         -- Price (Line 3)
        <*> validateArticle (lines !! 4)              -- Article (Line 4)
        <*> pure (T.unlines (drop 5 lines))           -- Description
        <*> pure PreCut
        <*> validateLength PreCut (lines !! 1) Nothing -- Length (Line 2)
        <*> validateWidth PreCut (lines !! 2)
        <*> pure isSearchable

--------------------------------------------------------------------------------
-- FIELD-LEVEL VALIDATORS & HELPERS
--------------------------------------------------------------------------------

-- | Validates an article string. Must contain only uppercase letters, numbers, and dashes.
validateArticle :: Text -> Validation [ParseError] Text
validateArticle articleRaw =
    let pattern = "^ART-[0-9]{1,8}$" :: String
    in if T.unpack articleRaw =~ pattern
        then Success articleRaw
        else Failure [InvalidArticleFormat articleRaw]

-- | Validates a price string, ensuring it starts with "Цена:".
validatePrice :: FabricType -> Text -> Validation [ParseError] Int
validatePrice fType raw =
  if "Цена" `T.isPrefixOf` raw
  then case extractInt raw of
         Just p  -> Success p
         Nothing -> Failure [ValueError fType ("Could not parse number from price line: " <> raw)]
  else Failure [ValueError fType ("Price line must start with 'Цена:'. Got: " <> raw)]

-- | Validates a length string, ensuring it starts with "Длина:".
validateLength :: FabricType -> Text -> Maybe Double -> Validation [ParseError] Double
validateLength fType raw threshold =
  if "Длина" `T.isPrefixOf` raw
 then case extractDouble raw of
        Just l  -> 
          if (fType == Roll && Just l >= threshold) || fType == PreCut then
            Success l
          else Failure [ValueError fType "length must be > 1 for rolls"]
        Nothing -> Failure [ValueError fType ("Could not parse number from length line: " <> raw)]
  else Failure [ValueError fType ("Length line must start with 'Длина:'. Got: " <> raw)]

-- Validate width
validateWidth :: FabricType -> Text -> Validation [ParseError] Int
validateWidth fType raw =
  if "Ширина" `T.isPrefixOf` raw
    then case extractInt raw of -- Width is usually an integer (cm)
      Just w  -> Success w
      Nothing -> Failure [ValueError fType ("Could not parse number from width line: " <> raw)]
  else Failure [ValueError fType ("Width line must start with 'Ширина:'. Got: " <> raw)]

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
renderValidationErrors :: [ParseError] -> Text
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
simpleErrorText :: ParseError -> Text
simpleErrorText (StructureError _ msg) = msg
simpleErrorText (ValueError _ msg) = msg
simpleErrorText (AmbiguousFormat msg) = msg
simpleErrorText (InvalidArticleFormat t) = "Invalid Article format: `" <> t <> "`. Use only A-Z, 0-9, and dashes (-)."
simpleErrorText MissingVisibilityTag = "Missing visibility tag. Please add either `#_search` (to make it searchable) or `#_nosearch` (to hide it)."