{-# LANGUAGE OverloadedStrings #-}

module Domain.Warehouse.Parser (parseIngestRequest, renderParseError) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Char as C
import Text.Read (readMaybe)
import Domain.Admin.Types

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

--------------------------------------------------------------------------------
-- MAIN LOGIC
--------------------------------------------------------------------------------

parseIngestRequest :: Text -> Either AdminParseError ParsedFabric
parseIngestRequest rawText = do
    let lines = filter (not . T.null) $ map T.strip $ T.lines rawText
    
    if null lines then Left UnknownType else Right ()

    -- 1. Detect Type
    let isPreCut = "#отрез" `T.isInfixOf` T.toLower rawText 
                || "#лоскут" `T.isInfixOf` T.toLower rawText

    -- 2. Route
    if isPreCut
        then parsePreCut lines
        else parseRoll lines

--------------------------------------------------------------------------------
-- PARSERS (Now throwing Typed Errors)
--------------------------------------------------------------------------------

parsePreCut :: [Text] -> Either AdminParseError ParsedFabric
parsePreCut lines = do
    if length lines < 4 
        then Left (StructureError PreCut "Need at least 4 lines") 
        else Right ()

    let name     = lines !! 0
    let lenRow   = lines !! 1
    let priceRow = lines !! 2
    let article  = lines !! 3
    let desc     = T.unlines (drop 4 lines)

    len <- case extractDouble lenRow of
        Just d  -> Right d
        Nothing -> Left (ValueError PreCut $ "Line 2 (Length) invalid: " <> lenRow)

    price <- case extractInt priceRow of
        Just p  -> Right p
        Nothing -> Left (ValueError PreCut $ "Line 3 (Price) invalid: " <> priceRow)

    Right $ ParsedFabric name price article desc PreCut (Just len)

parseRoll :: [Text] -> Either AdminParseError ParsedFabric
parseRoll lines = do
    if length lines < 3
        then Left (StructureError Roll "Need at least 3 lines") 
        else Right ()

    let name     = lines !! 0
    let priceRow = lines !! 1
    let article  = lines !! 2
    let desc     = T.unlines (drop 3 lines)

    price <- case extractInt priceRow of
        Just p  -> Right p
        Nothing -> Left (ValueError Roll $ "Line 2 (Price) invalid: " <> priceRow)

    Right $ ParsedFabric name price article desc Roll Nothing

--------------------------------------------------------------------------------
-- HELPERS (Extraction)
--------------------------------------------------------------------------------
-- Same extractInt/extractDouble as before...
extractInt :: Text -> Maybe Int
extractInt t = readMaybe (T.unpack $ T.filter C.isDigit t)

extractDouble :: Text -> Maybe Double
extractDouble t = 
    let norm = T.replace "," "." t
        clean = T.filter (\c -> C.isDigit c || c == '.') norm
    in readMaybe (T.unpack clean)

--------------------------------------------------------------------------------
-- ERROR RENDERING (The View Layer)
--------------------------------------------------------------------------------

renderParseError :: AdminParseError -> Text
renderParseError err = case err of
    
    -- Case: We couldn't even determine what the user wanted
    UnknownType -> 
        "❌ **Error: Empty Post or Unknown Type**\n\n" <>
        "Please use hashtags `#отрез` or ensure format is correct.\n\n" <>
        rollTemplate <> "\n\n" <> preCutTemplate

    -- Case: Pre-Cut Logic Failed
    ValueError PreCut msg -> 
        "❌ **Parsing Error (Pre-Cut)**\n" <>
        "_" <> msg <> "_\n\n" <>
        "👇 **Expected Format:**\n" <>
        preCutTemplate

    StructureError PreCut msg -> 
        "❌ **Layout Error (Pre-Cut)**\n" <>
        "_" <> msg <> "_\n\n" <>
        "👇 **Expected Format:**\n" <>
        preCutTemplate

    -- Case: Roll Logic Failed
    ValueError Roll msg -> 
        "❌ **Parsing Error (Roll)**\n" <>
        "_" <> msg <> "_\n\n" <>
        "👇 **Expected Format:**\n" <>
        rollTemplate

    StructureError Roll msg -> 
        "❌ **Layout Error (Roll)**\n" <>
        "_" <> msg <> "_\n\n" <>
        "👇 **Expected Format:**\n" <>
        rollTemplate