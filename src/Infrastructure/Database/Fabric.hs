{-# LANGUAGE QuasiQuotes #-}

module Infrastructure.Database.Fabric (ingestFabricDB) where

import qualified Hasql.Transaction as Hasql
import qualified Hasql.Transaction.Sessions as Hasql
import qualified Hasql.Statement as Hasql
import qualified Hasql.TH as Hasql
import Domain.Warehouse.Types (Fabric(..), FabricType(..))
import  API.Types (RawIngestRequest (..))
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Control.Monad (void, when)

import Text (encodeToText)


-- | Main Entry Point
ingestFabricDB :: Fabric -> RawIngestRequest -> Hasql.Transaction Int64
ingestFabricDB fabric req = do
  -- 1. Calculate 'Per Meter' properties for the Parent Fabric
  -- If it's a pre-cut, derive the meter price so the parent table stays consistent.
  let finalPricePerMeter = 
        case fType fabric of
          Roll -> Just (fPrice fabric)
          PreCut -> Nothing

  -- 2. UPSERT the Parent Fabric
  -- If Article exists: Update Name, Price, Description, Media.
  -- If New: Insert it.
  parentId <- 
    Hasql.statement (
      fArticle fabric,                 -- $1 Article (Unique Key)
      fName fabric,                    -- $2 Name
      fmap fromIntegral finalPricePerMeter :: Maybe Int32, -- $3 Price/m
      fDescription fabric,             -- $4 Desc
      rawMsgId req,                    -- $5 warehouse_msg_id
      rawFileId req,                   -- $6 warehouse_file_id (Thumb)
      rawMediaGroupId req,             -- $7 warehouse_media_group_id
      encodeToText (rawMediaType req), -- $8 warehouse_media_type
      if fType fabric == Roll 
      then Length fabric
      else 0.0                         -- $9 Length (Only for rolls)                
    ) upsertFabricQuery

  -- 3. If it is a Pre-Cut, insert the specific piece child row
  when(fType fabric == PreCut) $ do
    let len = fLength fabric
    let total = fPrice fabric
    void $ Hasql.statement (parentId, len, fromIntegral total :: Int32) insertPreCutQuery
  return parentId

-- -----------------------------------------------------------------------------
-- SQL QUERIES (Hasql TH)
-- -----------------------------------------------------------------------------

upsertFabricQuery :: Hasql.Statement (Text, Text, Maybe Int32, Text, Int64, Maybe Text, Maybe Text, Text, Double) Int64
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
      available_length_m
    ) 
    VALUES (
      $1 :: text, 
      $2 :: text, 
      coalesce($3 :: int4?, 0), 
      $4 :: text,
      $5 :: int8,
      $6 :: text?,
      $7 :: text?,
      $8 :: text,
      $9 :: float8,
      $9 :: float8
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
        updated_at = NOW()
    RETURNING id :: int8
|]

insertPreCutQuery :: Hasql.Statement (Int64, Double, Int32) ()
insertPreCutQuery = 
  [Hasql.resultlessStatement| 
    INSERT INTO pre_cuts (fabric_id, length_m, price_rub)
    VALUES ($1 :: int8, $2 :: float8, $3 :: int4)
|]