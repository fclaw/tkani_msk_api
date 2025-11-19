-- We need TemplateHaskell to generate functions from SQL files
{-# LANGUAGE TemplateHaskell       #-}
-- Also helpful for writing the TH splices
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE DataKinds             #-}

module Infrastructure.Database
  ( getFabricInfoById
  , putNewFabric
  , getFinalOrderItemPrice
  , placeNewOrder
  , module Types
  ) where


import qualified Hasql.Pool as Hasql
import qualified Hasql.Transaction as Hasql
import qualified Hasql.Transaction.Sessions as Hasql
import qualified Hasql.Statement as Hasql
import qualified Hasql.TH as TH
import Data.Profunctor.Unsafe (dimap, lmap)
import Data.Aeson (FromJSON, fromJSON, Result (..), Value)
import Data.Text (Text, pack)
import Data.Bifunctor (first, second)
import Control.Monad (join)
import Data.Tuple.Ops (initT, app2, app3)
import Data.Int (Int64)


import API.Types (FabricInfo(..), PreCut(..), FullFabric) -- Your data types
import TH.RecordToTuple (recordToTuple)
import API.WithField (WithField)
import qualified Infrastructure.Database.Types as Types
import Infrastructure.Database.Types (Order (..))


--------------------------------------------------------------------------------
-- Template Haskell Magic: Generate our statement functions automatically
--------------------------------------------------------------------------------

convertFromJson :: FromJSON a => Value -> Either String a
convertFromJson value =
  case fromJSON value of
   Success fabricInfo -> Right fabricInfo
   Error msg -> Left msg


runTransaction pool mode = Hasql.use pool . Hasql.transaction Hasql.Serializable mode

-- | Statement to fetch a single fabric row by its ID.
--   TH.singletonStatement reads the SQL, infers the parameter and result types.
getFabricStatement :: Hasql.Statement Int (Either String FullFabric)
getFabricStatement =
  dimap fromIntegral (maybe (Left "fabric not found") id . fmap (convertFromJson @FullFabric))
  [TH.maybeStatement|
    select json_build_object(
      'fiId', f.id,
      'fiDescription', f.description,
      'fiTotalLengthM', f.total_length_m,
      'fiPricePerMeter', f.price_per_meter,
      'fiAvailableLengthM', f.available_length_m,
      'fiIsSold', f.is_sold,
      'fiArticle', f.article,
      'fiPreCuts', coalesce((
        select jsonb_agg(
          json_build_object(
          'pcId', pc.id,
          'pcLengthM', pc.length_m,
          'pcPriceRub', pc.price_rub,
          'pcInStock', pc.in_stock)
         ) :: jsonb
         from pre_cuts as pc
         where 
          pc.fabric_id = f.id and 
          pc.in_stock = TRUE),
         '[]'::jsonb)) :: jsonb
    from fabrics as f
    where f.id = $1 :: int4
  |]


-- | Fetches a fabric and all its associated, in-stock pre-cuts from the database.
getFabricInfoById :: Int -> Hasql.Pool -> IO (Either Text FullFabric)
getFabricInfoById fabricId_ pool = 
  fmap (join . first (pack . show)) $ 
    runTransaction pool Hasql.Read $ 
      fmap (first pack) $ fabricId_ `Hasql.statement` getFabricStatement

putNewFabricStatement :: Hasql.Statement FabricInfo Int
putNewFabricStatement = 
  dimap (app2 fromIntegral . app3 fromIntegral . initT . $(recordToTuple ''FabricInfo)) fromIntegral
  [TH.singletonStatement|
    insert into fabrics 
    (description, 
     total_length_m, 
     price_per_meter, 
     available_length_m,
     article)
    values (
      $1 :: text, 
      $2 :: int4, 
      $3 :: int4, 
      $4 :: float8,
      $5 :: text)
    returning id :: int4
  |]

putNewFabric :: FabricInfo -> Hasql.Pool -> IO (Either Text Int)
putNewFabric fabricInfo_ pool = 
  fmap (first (pack . show)) $ 
    runTransaction pool Hasql.Write $ 
      fabricInfo_ `Hasql.statement` putNewFabricStatement


getFinalOrderItemPriceStatement :: Hasql.Statement (Int64, Maybe Int64, Maybe Double) Double
getFinalOrderItemPriceStatement = 
  [TH.singletonStatement|
    select
      (case
        when $2 :: int8? is not null then
          (select pc.price_rub
           from pre_cuts pc
           where pc.id = $2 :: int8? and pc.fabric_id = $1 :: int8)
        when $2 :: int8? is not null and $3 :: float8? is not null then
          (select f.price_per_meter * $3 :: float8?
           from fabrics f
           where f.id = $1 :: int8)
        else 0.0
      end) :: float8
    from fabrics
    where id = $1 :: int8
  |]

-- | Fetches the final, calculated price for a fabric order item.
--   The entire calculation (per-meter vs. fixed price) is handled by the SQL query.
--   Returns 'Nothing' if the fabric or pre-cut is not found.
getFinalOrderItemPrice :: Int64 -> Maybe Int64 -> Maybe Double -> Hasql.Pool -> IO (Either Text Double)
getFinalOrderItemPrice fabricId preCutId lengthM pool = 
  fmap (first (pack . show)) $
    runTransaction pool Hasql.Write $ 
      params `Hasql.statement` getFinalOrderItemPriceStatement
  where params = (fromIntegral fabricId, fmap fromIntegral preCutId, lengthM)


placeNewOrderStatement :: Hasql.Statement Order ()
placeNewOrderStatement = 
  dimap $(recordToTuple ''Order) (const ())
  [TH.singletonStatement|
    insert into orders (
      id,
      fabric_id,
      length_m,
      pre_cut_id,
      customer_full_name,
      customer_phone,
      delivery_provider_id,
      delivery_point_id,
      telegram_url,
      sdek_request_uuid,
      sdek_tracking_number,
      internal_notification_message_id,
      created_at,
      updated_at,
      status
    ) values (
      $1 :: text, 
      $2 :: int8, 
      $3 :: float8?, 
      $4 :: int8?,
      $5 :: text,
      $6 :: text,
      $7 :: text,
      $8 :: text,
      $9 :: text,
      $10 :: uuid,
      $11 :: text,
      $12 :: int8,
      now(),
      now(),
      'registered'
      ) returning id :: text
  |]

placeNewOrder :: Order -> Hasql.Pool -> IO (Either Text ())
placeNewOrder order pool =
  fmap (first (pack . show)) $
    runTransaction pool Hasql.Write $ 
      order `Hasql.statement` placeNewOrderStatement