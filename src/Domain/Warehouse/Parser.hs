{-# LANGUAGE OverloadedStrings #-}

module Domain.Warehouse.Parser (parseIngestRequest, renderValidationErrors) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Char as C
import Text.Read (readMaybe)
import Data.Validation (Validation(..), toEither)
import Text.Regex.TDFA ((=~)) -- Import the regex matcher

import Domain.Warehouse.Types -- Your types: Fabric, FabricType, AdminParseError

--------------------------------------------------------------------------------
-- TEMPLATES (For Error Messages)
--------------------------------------------------------------------------------
rollTemplate :: Text
rollTemplate = 
    "📄 **Standard Roll Format**\n" <>
    "`Name \\(Line 1\\)`\n" <>
    "`Цена: 1500 руб/метр \\(Line 2\\)`\n" <>
    "`ART\\-123 \\(Line 3\\)`\n" <>
    "`Description \\.\\.\\.`"

preCutTemplate :: Text
preCutTemplate = 
    "✂️ **PreCut Format \\(\\#отрез\\)**\n" <>
    "`Name \\(Line 1\\)`\n" <>
    "`Длина: 1\\.2 м \\(Line 2\\)`\n" <>
    "`Цена: 2400 руб \\(Line 3\\)`\n" <>
    "`ART\\-123 \\(Line 4\\)`\n" <>
    "`Description \\.\\.\\.`"

--------------------------------------------------------------------------------
-- MAIN LOGIC
--------------------------------------------------------------------------------

-- | This function now becomes the brain. It *decides* the type, it doesn't just check tags.
parseIngestRequest :: Text -> Either [AdminParseError] Fabric
parseIngestRequest rawText =
    let
      cleanLines = filter (not . T.null) $ map T.strip $ T.lines rawText
    in 
      toEither $
        if null cleanLines then 
          Failure [AmbiguousFormat "Post is too short\\. At least 3 lines are required"]
        else
            -- 1. Try to parse as Pre-Cut FIRST
            case toEither (validatePreCut cleanLines) of    
               -- Success: We are sure it's a pre-cut, we are done.
              Right result -> Success result  
              -- Failure: It's NOT a Pre-Cut. Let's see if it's a Roll.
              Left preCutErrors ->
                case toEither (validateRoll cleanLines) of
                  -- Success: Okay, it must be a Roll.
                  Right result -> Success result
                  -- Failure: It's not a Pre-Cut AND not a Roll.
                  Left rollErrors ->
                    -- Now we give the most helpful error.
                    -- If a tag was present, the user INTENDED it to be a pre-cut,
                    -- so show the pre-cut error.
                    let hasTag = "#отрез" `T.isInfixOf` T.toLower rawText
                    in if hasTag
                        then Failure preCutErrors
                        else Failure (preCutErrors <> rollErrors) -- Show ALL errors

--------------------------------------------------------------------------------
-- VALIDATORS (Applicative Style)
--------------------------------------------------------------------------------

-- | Validator for the Roll pattern
validateRoll :: [Text] -> Validation [AdminParseError] Fabric
validateRoll lines =
  -- NEW GUARD: If this post contains "Длина", it is NOT a Roll.
  if any ("Длина" `T.isPrefixOf`) lines
    then Failure [StructureError Roll "Found a 'Length' line\\. If this is a PreCut, please add \\#отрез tag"]
  else 
    if length lines < 3 then 
      Failure [StructureError Roll "Need at least 3 lines for a Roll"]
    else
      Fabric
        <$> pure (lines !! 0)                 -- Name (Line 1)
        <*> validatePrice (lines !! 1)        -- Price (Line 2)
        <*> validateArticle (lines !! 2)      -- Article (Line 3) - NOW VALIDATED
        <*> pure (T.unlines (drop 3 lines))   -- Description
        <*> pure Roll
        <*> pure Nothing                      -- Length is always Nothing for Rolls

-- | Validator for the Pre-Cut pattern
validatePreCut :: [Text] -> Validation [AdminParseError] Fabric
validatePreCut lines =
  -- NEW GUARD: If NO "Длина" line, it is NOT a Pre-Cut
  if not (any ("Длина" `T.isPrefixOf`) lines)
  then Failure [StructureError PreCut "Missing 'Длина: \\.\\.\\.' line for PreCut"]
  else 
    if length lines < 4 then 
      Failure [StructureError PreCut "Need at least 4 lines for a PreCut"]
    else
      Fabric
        <$> pure (lines !! 0)                         -- Name (Line 1)
        <*> validatePrice (lines !! 2)                -- Price (Line 3)
        <*> validateArticle (lines !! 3)              -- Article (Line 4) - NOW VALIDATED
        <*> pure (T.unlines (drop 4 lines))           -- Description
        <*> pure PreCut
        <*> (Just <$> validateLength (lines !! 1))    -- Length (Line 2)

--------------------------------------------------------------------------------
-- FIELD-LEVEL VALIDATORS & HELPERS
--------------------------------------------------------------------------------

-- | Validates an article string. Must contain only letters, numbers, and dashes.
validateArticle :: Text -> Validation [AdminParseError] Text
validateArticle articleRaw = 
    -- This regex pattern allows uppercase letters, numbers, and dashes.
    let pattern = "^[A-Z0-9-]+$" :: String
    in if T.unpack articleRaw =~ pattern
        then Success articleRaw
        else Failure [InvalidArticleFormat articleRaw]

-- | Generic helper to run a read function and wrap the result in Validation.
validateRead :: (Text -> Maybe a) -> (Text -> AdminParseError) -> Text -> Validation [AdminParseError] a
validateRead reader errorConstructor raw =
    case reader raw of
        Just val -> Success val
        Nothing  -> Failure [errorConstructor raw]

-- | Validates a price string, but also checks for "Цена:" prefix for context.
validatePrice :: Text -> Validation [AdminParseError] Int
validatePrice raw = 
    if "Цена" `T.isPrefixOf` raw
    then
        let digits = T.filter C.isDigit raw
        in validateRead (readMaybe . T.unpack) (\t -> InvalidPrice raw) digits
    else
        Failure [InvalidPrice $ "Missing 'Цена:' prefix on line: " <> raw]

-- | Validates a length string, checking for "Длина:" prefix.
validateLength :: Text -> Validation [AdminParseError] Double
validateLength raw = 
    if "Длина" `T.isPrefixOf` raw
    then
        let norm = T.replace "," "." raw
            clean = T.filter (\c -> C.isDigit c || c == '.') norm
        in validateRead (readMaybe . T.unpack) (\t -> InvalidLength raw) clean
    else
        Failure [InvalidLength $ "Missing 'Длина:' prefix on line: " <> raw]

--------------------------------------------------------------------------------
-- ERROR RENDERING (The part that generates the nice message for the Bot)
--------------------------------------------------------------------------------

-- In Domain/Admin/Parser.hs

-- | Takes a list of errors and formats them into a single, user-friendly message.
renderValidationErrors :: [AdminParseError] -> Text
renderValidationErrors errors =
    -- Check if any error is the generic "Ambiguous" one
    let isAmbiguous = any isAmbiguousError errors
        isAmbiguousError (AmbiguousFormat _) = True
        isAmbiguousError _ = False

        -- Or check if it's a pre-cut specific error
        isPreCutError = any isPreCutContext errors
        isPreCutContext (StructureError PreCut _) = True
        isPreCutContext (ValueError PreCut msg) = "Length" `T.isInfixOf` msg -- Heuristic
        isPreCutContext _ = False
    in
    if isAmbiguous
        then renderAmbiguousError errors -- Show both templates
        else
            let
                header = "❌ **Parsing Errors Found:**\n\n"
                errorText = T.intercalate "\n" $ map ("• " <>) $ map simpleErrorText errors
                template = if isPreCutError then preCutTemplate else rollTemplate
                -- Add a helpful hint if the user might have forgotten the tag
                hint = if not isPreCutError 
                       then "\n\nPS: If this was a precut, please add the `#отрез` tag to help me identify it next time\\!"
                       else ""
            in
            header <> errorText <> "\n\n👇 **Expected Format:**\n" <> template <> hint

-- | Specific renderer for when we can't even guess the type.
renderAmbiguousError :: [AdminParseError] -> Text
renderAmbiguousError errors =
    "❌ **Format Error:** " <> (simpleErrorText $ head errors) <> "\n\n" <>
    "I could not determine if this is a Roll or a PreCut\\. Please check the formats below:\n\n" <>
    rollTemplate <> "\n\n" <> preCutTemplate

-- | Renders a single error from the list into a simple string.
simpleErrorText :: AdminParseError -> Text
simpleErrorText err = case err of
    StructureError _ msg   -> msg
    ValueError _ msg       -> msg
    AmbiguousFormat msg    -> msg
    InvalidPrice raw       -> "Invalid price value: `" <> raw <> "`"
    InvalidLength raw      -> "Invalid length value: `" <> raw <> "`"
    InvalidArticleFormat t -> "Invalid Article format: `" <> t <> "`\\. Use only letters \\(A\\-Z\\), numbers \\(0\\-9\\), and dashes \\(\\-\\)\\."