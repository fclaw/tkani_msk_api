{-# LANGUAGE OverloadedStrings #-}

module Domain.Warehouse.Parser (parseIngestRequest) where

import Text.Read (readMaybe)
import Domain.Warehouse.Types
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Char as C
import Data.Validation (Validation(..))
import GHC.Generics (Generic)
import Text.Read (readMaybe)
import Control.Applicative (liftA2)

--------------------------------------------------------------------------------
-- TEMPLATES
--------------------------------------------------------------------------------

rollTemplate :: Text
rollTemplate = 
    "📄 **Standard Roll Format**\n" <>
    "`Name (Line 1)`\n" <>
    "`Price: 1500 (Line 2)`\n" <>
    "`ART-123 (Line 3)`\n" <>
    "`Description...`\n" <>
    "`#tag`"

preCutTemplate :: Text
preCutTemplate = 
    "✂️ **Pre-Cut Format (#отрез)**\n" <>
    "`Name (Line 1)`\n" <>
    "`Length: 1.2 (Line 2)`\n" <>
    "`Total Price: 2400 (Line 3)`\n" <>
    "`ART-123 (Line 4)`\n" <>
    "`Description...`\n" <>
    "`#отрез`"


-- | Helper: Validates a reading function
-- If read succeeds: Success a
-- If read fails: Failure [Error]
validateRead :: (Text -> Maybe a) -> (Text -> AdminParseError) -> Text -> Validation [AdminParseError] a
validateRead reader errorBuilder raw =
    case reader raw of
        Just x  -> Success x
        Nothing -> Failure [errorBuilder raw]

-- | 1. Main Entry Point
-- Note: We return 'Validation [Error] Result' instead of 'Either Error Result'
parseIngestRequest :: Text -> Validation [AdminParseError] ParsedFabric
parseIngestRequest rawText = 
    let 
        cleanLines = filter (not . T.null) $ map T.strip $ T.lines rawText
        isPreCut = "#отрез" `T.isInfixOf` T.toLower rawText 
                || "#лоскут" `T.isInfixOf` T.toLower rawText
    in 
    -- We still need a "Fail Fast" check for line counts, 
    -- because we can't validate Line 4 if it doesn't exist.
    if isPreCut 
        then validatePreCut cleanLines 
        else validateRoll cleanLines

--------------------------------------------------------------------------------
-- PARSERS (Using Applicative Style)
--------------------------------------------------------------------------------

validatePreCut :: [Text] -> Validation [AdminParseError] ParsedFabric
validatePreCut lines 
    | length lines < 4 = Failure [NotEnoughLines PreCut (length lines)]
    | otherwise = 
        ParsedFabric 
            <$> pure (lines !! 0)                      -- Name (Line 1) - Always Valid
            <*> validatePrice (lines !! 2)             -- Price (Line 3)
            <*> pure (lines !! 3)                      -- Article (Line 4)
            <*> pure (T.unlines (drop 4 lines))        -- Description
            <*> pure PreCut
            <*> (Just <$> validateLength (lines !! 1)) -- Length (Line 2)

validateRoll :: [Text] -> Validation [AdminParseError] ParsedFabric
validateRoll lines 
    | length lines < 3 = Failure [NotEnoughLines Roll (length lines)]
    | otherwise =
        ParsedFabric
            <$> pure (lines !! 0)                 -- Name
            <*> validatePrice (lines !! 1)        -- Price
            <*> pure (lines !! 2)                 -- Article
            <*> pure (T.unlines (drop 3 lines))   -- Description
            <*> pure Roll
            <*> pure Nothing                      -- Length is Nothing

--------------------------------------------------------------------------------
-- FIELD VALIDATORS
--------------------------------------------------------------------------------

validatePrice :: Text -> Validation [AdminParseError] Int
validatePrice raw = 
    let digits = T.filter C.isDigit raw
    in validateRead (readMaybe . T.unpack) InvalidPrice digits

validateLength :: Text -> Validation [AdminParseError] Double
validateLength raw = 
    let norm = T.replace "," "." raw
        clean = T.filter (\c -> C.isDigit c || c == '.') norm
    in validateRead (readMaybe . T.unpack) InvalidLength clean

--------------------------------------------------------------------------------
-- ERROR RENDERING (Handling Multiple Errors)
--------------------------------------------------------------------------------

-- | Converts a list of errors into a single message string for Telegram
renderValidationErrors :: [AdminParseError] -> Text
renderValidationErrors errors = 
    let 
        -- Helper to detect context (PreCut vs Roll) from the errors to show right template
        isPreCutError = any checkType errors
        checkType (InvalidLength _) = True
        checkType (NotEnoughLines PreCut _) = True
        checkType _ = False

        header = "❌ **Parsing Errors Found:**\n"
        
        -- Render each error as a bullet point
        errorText = T.intercalate "\n" $ map ("• " <>) $ map simpleErrorText errors
        
        template = if isPreCutError then preCutTemplate else rollTemplate
    in 
    header <> errorText <> "\n\n👇 **Expected Format:**\n" <> template

-- Simpler render for individual list items
simpleErrorText :: AdminParseError -> Text
simpleErrorText err = case err of
    NotEnoughLines Roll count -> "Need at least 3 lines. Missing lines (Found " <> T.pack (show count) <> ")"
    NotEnoughLines PreCut count -> "Need at least 4 lines for #отрез. Missing lines (Found " <> T.pack (show count) <> ")"
    InvalidPrice t       -> "Invalid Price number: " <> t
    InvalidLength t      -> "Invalid Length number: " <> t
    MissingTag           -> "Missing Type Tag"
    _                    -> "Unknown formatting error"