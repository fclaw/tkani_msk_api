{-# LANGUAGE QuasiQuotes #-}

module Infrastructure.Database.Fabric (ingestFabricDB) where

import Data.Aeson (Value, toJSON)
import qualified Hasql.Transaction as Hasql
import qualified Hasql.Transaction.Sessions as Hasql
import qualified Hasql.Statement as Hasql
import qualified Hasql.TH as Hasql
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Maybe (fromMaybe, isJust)
import Control.Monad (void, when)
import Data.Time (Day)
import Data.Hashable (hash)

import Text (encodeToText)
import Domain.Warehouse.Types (Fabric(..), FabricType(..))
import  API.Types (RawIngestRequest (..), fpDensity, fpWeightPerMetre)


-- | Main Entry Point
ingestFabricDB :: Fabric -> RawIngestRequest -> Hasql.Transaction (Int64, Text)
ingestFabricDB fabric req = do
  -- 1. Calculate 'Per Meter' properties for the Parent Fabric
  -- If it's a pre-cut, derive the meter price so the parent table stays consistent.
  let finalPricePerMeter = 
        case fType fabric of
          Roll -> Just (fPrice fabric)
          PreCut -> Nothing

  let fabricHash = abs $ fromIntegral $ hash fabric

  -- 2. UPSERT the Parent Fabric
  -- If Article exists: Update Name, Price, Description, Media.
  -- If New: Insert it.
  let rollLength | fType fabric == Roll = fLength fabric
                 | otherwise = 0.0
  (parentId, article) <- 
    Hasql.statement (
      fArticle fabric,                 -- $1 Article (Unique Key)
      fName fabric,                    -- $2 Name
      fmap fromIntegral finalPricePerMeter :: Maybe Int32, -- $3 Price/m
      fDescription fabric,             -- $4 Desc
      rawMsgId req,                    -- $5 warehouse_msg_id
      rawFileId req,                   -- $6 warehouse_file_id (Thumb)
      rawMediaGroupId req,             -- $7 warehouse_media_group_id
      encodeToText (rawMediaType req), -- $8 warehouse_media_type
      rollLength,                      -- $9 Length (Only for rolls)
      rawThumbnailUrl req,             -- $10 preview on a search list
      fromIntegral (fWidth fabric),    -- $11 the width of a fabric
      fIsSearchable fabric && 
      fType fabric == Roll,
      encodeToText (fpDensity (rawFabricProperties req)),
      fpWeightPerMetre (rawFabricProperties req),
      fabricHash,
      toJSON (rawComposition req)
    ) upsertFabricQuery

  -- 3. If it is a Pre-Cut, insert the specific piece child row
  precutRes <-
    if fType fabric == PreCut then do
      let len = fLength fabric
      let total = fPrice fabric
      let isSearchable = 
             fIsSearchable fabric && 
             fType fabric == PreCut
      fmap Just $ Hasql.statement (parentId, len, fromIntegral total :: Int32, isSearchable) insertPreCutQuery
    else return Nothing

  return $ case precutRes of
    Nothing -> (parentId, article)
    Just (precutId) -> (precutId, article)

-- -----------------------------------------------------------------------------
-- SQL QUERIES (Hasql TH)
-- -----------------------------------------------------------------------------

type RawFabric = 
     ( Maybe Text
     , Text
     , Maybe Int32
     , Text
     , Int64
     , Maybe Text
     , Maybe Text
     , Text
     , Double
     , Maybe Text
     , Int32
     , Bool
     , Text
     , Double
     , Int64
     , Value)

upsertFabricQuery :: Hasql.Statement RawFabric (Int64, Text)
upsertFabricQuery = 
  [Hasql.singletonStatement|
    INSERT INTO fabrics (
      article, 
      name, 
      price_per_meter, 
      description,
      warehouse_message_id,
      image_url,
      media_group_id,
      media_type,
      total_length_m,
      available_length_m,
      thumbnail_url,
      width,
      is_searchable,
      density,
      weight_per_metre,
      hash,
      composition
    ) 
    VALUES (
      COALESCE($1 :: text?, next_fabric_article()),
      $2 :: text, 
      coalesce($3 :: int4?, 0), 
      $4 :: text,
      $5 :: int8,
      $6 :: text?,
      $7 :: text?,
      $8 :: text,
      $9 :: float8,
      $9 :: float8,
      $10 :: text?,
      $11 :: int4,
      $12 :: bool,
      CAST($13 :: text AS fabric_density),
      $14 :: float8,
      $15 :: int8,
      $16 :: jsonb
    )
    ON CONFLICT (article) DO UPDATE
    SET 
        name = EXCLUDED.name,
        price_per_meter = EXCLUDED.price_per_meter,
        description = EXCLUDED.description,
        warehouse_message_id = EXCLUDED.warehouse_message_id,
        image_url = EXCLUDED.image_url,
        media_group_id = EXCLUDED.media_group_id,
        media_type = EXCLUDED.media_type,
        total_length_m = 
          fabrics.total_length_m + 
          COALESCE(EXCLUDED.total_length_m, 0),
        available_length_m = 
          fabrics.available_length_m + 
          COALESCE(EXCLUDED.available_length_m, 0),
        thumbnail_url = EXCLUDED.thumbnail_url,
        width = EXCLUDED.width,
        is_searchable = EXCLUDED.is_searchable,
        updated_at = NOW(),
        in_stock = TRUE,
        is_sold = FALSE,
        density = EXCLUDED.density,
        weight_per_metre = EXCLUDED.weight_per_metre,
        hash = EXCLUDED.hash,
        composition = EXCLUDED.composition
    RETURNING id :: int8, article :: text
  |]

insertPreCutQuery :: Hasql.Statement (Int64, Double, Int32, Bool) Int64
insertPreCutQuery = 
  [Hasql.singletonStatement| 
    INSERT INTO pre_cuts 
    (fabric_id, 
     length_m, 
     price_rub, 
     is_searchable)
    VALUES (
      $1 :: int8, 
      $2 :: float8, 
      $3 :: int4, 
      $4 :: bool
    )
    ON CONFLICT (fabric_id, length_m, price_rub) 
    DO UPDATE SET
      is_searchable = EXCLUDED.is_searchable,
      in_stock = TRUE
    RETURNING id :: int8
  |]