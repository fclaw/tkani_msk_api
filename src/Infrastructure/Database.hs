-- We need TemplateHaskell to generate functions from SQL files
{-# LANGUAGE TemplateHaskell       #-}
-- Also helpful for writing the TH splices
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TupleSections         #-}

module Infrastructure.Database
  ( getFabricInfoById
  , putNewFabric
  , getFinalOrderItemPrice
  , placeNewOrder
  , setTelegramMessage
  , getChatDetails
  , updateOrderStatusStatement
  , updateOrderStatus
  , adjustFabric
  , runTransaction
  , fetchOrderStatus
  , getOrdersInTransit
  , module Types
  ) where


import qualified Hasql.Pool as Hasql
import qualified Hasql.Transaction as Hasql
import qualified Hasql.Transaction.Sessions as Hasql
import qualified Hasql.Statement as Hasql
import qualified Hasql.TH as TH
import Data.Profunctor.Unsafe (dimap, lmap, rmap)
import Data.Aeson (FromJSON, fromJSON, Result (..), Value, fromJSON, Result)
import Data.Text (Text, pack)
import Data.Bifunctor (first, second)
import Control.Monad (join)
import Data.Tuple.Ops (initT, app2, app3)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.UUID (UUID)
import qualified Data.Vector as V
import Data.Either (fromRight)


import API.Types 
       ( FabricInfo(..)
       , PreCut(..)
       , FullFabric
       , SetTelegramMessageRequest (..)
       , OrderStatus
       , Providers
       , statusToSQL
       ) -- Your data types
import TH.RecordToTuple (recordToTuple)
import API.WithField (WithField)
import qualified Infrastructure.Database.Types as Types
import Infrastructure.Database.Types as Types
import Text (encodeToText)



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
getFabricStatement :: Hasql.Statement (Int64, Double) (Either String FullFabric)
getFabricStatement =
  dimap (first fromIntegral) (maybe (Left "fabric not found") id . fmap (convertFromJson @FullFabric))
  [TH.maybeStatement|
    SELECT jsonb_build_object(
        'fiId', f.id,
        'fiDescription', f.description,
        'fiTotalLengthM', CAST(f.total_length_m AS int4),
        'fiPricePerMeter', f.price_per_meter,
        'fiAvailableLengthM', f.available_length_m,
        'fiIsSold', f.is_sold,
        'fiArticle', f.article,
        'fiPreCuts', pc_data.json_val
    ) :: jsonb
    FROM fabrics AS f
    CROSS JOIN LATERAL (
        SELECT coalesce(
            jsonb_agg(
                jsonb_build_object(
                    'pcId', pc.id,
                    'pcLengthM', pc.length_m,
                    'pcPriceRub', pc.price_rub,
                    'pcInStock', pc.in_stock
                )
            ),
            '[]'::jsonb
        ) AS json_val
        FROM pre_cuts AS pc
        WHERE pc.fabric_id = f.id 
          AND pc.in_stock = TRUE
    ) AS pc_data

    WHERE 
      f.id = $1 :: int8 
      AND (
        f.available_length_m >= $2 :: float8
        
        OR 
        
        (
          f.available_length_m > 0.1 AND
          f.available_length_m < $2 :: float8 AND
          jsonb_array_length(pc_data.json_val) > 0
        )
      )
  |]


-- | Fetches a fabric and all its associated, in-stock pre-cuts from the database.
getFabricInfoById :: Int64 -> Double -> Hasql.Pool -> IO (Either Text (Either Text FullFabric))
getFabricInfoById fabricId threshold pool = 
  fmap (first (pack . show)) $
    runTransaction pool Hasql.Read $ 
      fmap (first pack) $ (fabricId, threshold) `Hasql.statement` getFabricStatement

putNewFabricStatement :: Hasql.Statement FabricInfo Int64
putNewFabricStatement = 
  dimap (app2 fromIntegral . app3 fromIntegral . initT . $(recordToTuple ''FabricInfo)) fromIntegral
  [TH.singletonStatement|
    INSERT INTO fabrics 
    (description, 
     total_length_m, 
     price_per_meter, 
     available_length_m,
     article)
    VALUES (
      $1 :: text, 
      $2 :: int4, 
      $3 :: int4, 
      $4 :: float8,
      $5 :: text)
    RETURNING id :: int8
  |]

putNewFabric :: FabricInfo -> Hasql.Pool -> IO (Either Text Int64)
putNewFabric fabricInfo_ pool = 
  fmap (first (pack . show)) $ 
    runTransaction pool Hasql.Write $ 
      fabricInfo_ `Hasql.statement` putNewFabricStatement


getFinalOrderItemPriceStatement :: Hasql.Statement (Int64, Maybe Int64, Maybe Double) Double
getFinalOrderItemPriceStatement = 
  [TH.singletonStatement|
    SELECT
      (CASE
        WHEN $2 :: int8? is not null THEN
          (SELECT pc.price_rub
           FROM pre_cuts pc
           WHERE pc.id = $2 :: int8? AND 
           pc.fabric_id = $1 :: int8)
        WHEN $2 :: int8? is null AND 
             $3 :: float8? is not null THEN
          (SELECT f.price_per_meter * $3 :: float8?
           FROM fabrics f
           WHERE f.id = $1 :: int8)
        ELSE 0.0
      END) :: float8
    FROM fabrics
    WHERE id = $1 :: int8
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
    WITH inserted_order AS (
      INSERT INTO orders (
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
      ) VALUES (
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
      )
      RETURNING id, fabric_id, length_m, pre_cut_id
    )
    INSERT INTO order_fabric_bindings (
        order_id, 
        fabric_id, 
        length_m,
        pre_cut_id
    ) 
    SELECT 
        id, 
        fabric_id, 
        length_m, 
        pre_cut_id 
    FROM inserted_order
    RETURNING order_id :: text
  |]

placeNewOrder :: Order -> Hasql.Pool -> IO (Either Text ())
placeNewOrder order pool = fmap (first (pack . show)) $ runTransaction pool Hasql.Write $ order `Hasql.statement` placeNewOrderStatement

setTelegramMessageStatement :: Hasql.Statement SetTelegramMessageRequest Int64
setTelegramMessageStatement =
   lmap (app3 fromIntegral . $(recordToTuple ''SetTelegramMessageRequest))
   [TH.rowsAffectedStatement| 
     INSERT INTO order_telegram_bindings 
     (order_id, chat_id, message_id) 
     VALUES ($1 :: text, $2 :: int8, $3 :: int4) |]

setTelegramMessage :: SetTelegramMessageRequest -> Hasql.Pool -> IO (Either Text Int64)
setTelegramMessage message pool = fmap (first (pack . show)) $ runTransaction pool Hasql.Write $ message `Hasql.statement` setTelegramMessageStatement

getChatDetailsStatement :: Hasql.Statement Text (Maybe Int)
getChatDetailsStatement = rmap (fmap fromIntegral) [TH.maybeStatement| SELECT message_id :: int FROM order_telegram_bindings WHERE order_id = $1 :: text |]

getChatDetails :: Text -> Hasql.Pool -> IO (Either Text (Maybe Int))
getChatDetails orderId pool = fmap (first (pack . show)) $ runTransaction pool Hasql.Read $ orderId `Hasql.statement` getChatDetailsStatement


updateOrderStatusStatement :: Hasql.Statement (Text, OrderStatus) Int
updateOrderStatusStatement = 
  dimap (second statusToSQL) fromIntegral
  [TH.singletonStatement| 
    UPDATE orders 
    SET status = CAST($2 :: text AS order_status) 
    WHERE id = $1 :: text 
    RETURNING internal_notification_message_id :: int4
  |]

updateOrderStatus :: Text -> OrderStatus -> Hasql.Pool -> IO (Either Text Int)
updateOrderStatus orderId status pool = fmap (first (pack . show)) $ runTransaction pool Hasql.Write $ (orderId, status) `Hasql.statement` updateOrderStatusStatement

-- | Updates inventory logic.
-- Logic details:
-- 1. order_info CTE: Fetches order details (works for Rolls and Pre-Cuts).
-- 2. update_pc CTE: Marks pre-cut as sold (only if it is a pre-cut).
-- 3. UPDATE fabrics:
--    - Subtracts length only if it was a Roll purchase.
--    - Calculates 'is_sold' based on zero length (for Rolls) or no-siblings (for Pre-Cuts).
-- 4. Returns JSON with new status and flags for admin notification.
adjustFabric :: Hasql.Statement (Text, Double) (Result AdjustFabric)
adjustFabric =
  rmap (fromJSON @AdjustFabric)
  [TH.singletonStatement|
    WITH order_info AS (
        SELECT
            ofb.fabric_id, 
            ofb.length_m, 
            ofb.pre_cut_id AS pre_cut_id 
        FROM order_fabric_bindings ofb
        WHERE ofb.order_id = $1 :: text
        LIMIT 1
    ),

    update_pc AS (
        UPDATE pre_cuts 
        SET in_stock = FALSE 
        FROM order_info
        WHERE pre_cuts.id = order_info.pre_cut_id
    )

    UPDATE fabrics f
    SET 
        available_length_m = CASE 
            WHEN order_info.pre_cut_id IS NULL 
            THEN f.available_length_m - order_info.length_m
            ELSE f.available_length_m
        END,

        is_sold = CASE 
            WHEN order_info.pre_cut_id IS NULL THEN 
                 (f.available_length_m - order_info.length_m) <= 0.01
            
            ELSE NOT EXISTS (
                SELECT 1 FROM pre_cuts pc 
                WHERE pc.fabric_id = order_info.fabric_id 
                  AND pc.in_stock = TRUE 
                  AND pc.id <> order_info.pre_cut_id 
            )
        END

    FROM order_info
    WHERE f.id = order_info.fabric_id

    RETURNING
        jsonb_build_object(
            'name', f.description :: text,
            'article', f.article :: text,
            'is_sold', f.is_sold :: bool,
            
            'is_pre_cut_req', (
                order_info.pre_cut_id IS NULL AND 
                f.available_length_m > 0.01 AND 
                f.available_length_m < $2 :: float8
            ) :: bool,
            
            'rem_length', f.available_length_m :: float8
        ) :: jsonb
  |]

fetchOrderStatus :: Text -> Hasql.Pool -> IO (Either Text (Maybe (OrderStatus, Text, Text, Providers)))
fetchOrderStatus query pool = fmap (join . first (pack . show)) $ runTransaction pool Hasql.Read $ query `Hasql.statement` fetchOrderStatusStatement


fetchOrderStatusStatement :: Hasql.Statement Text (Either Text (Maybe (OrderStatus, Text, Text, Providers)))
fetchOrderStatusStatement =
  rmap (sequence . fmap (first pack) . fmap convert)
  [TH.maybeStatement|
    SELECT 
      to_jsonb(CAST(status AS text)) :: jsonb,
      id :: text,
      sdek_tracking_number :: text,
      to_jsonb(delivery_provider_id) :: jsonb
    FROM orders
    WHERE
      id = $1 :: text OR
      sdek_tracking_number = $1 :: text
  |]
  where
    convert (jsonStatus, orderId, trackingN, jsonProvider) = do
      status <- convertFromJson @OrderStatus jsonStatus
      provider <- convertFromJson @Providers jsonProvider
      return (status, orderId, trackingN, provider)

getOrdersInTransit :: [OrderStatus] -> Hasql.Pool -> IO (Either Text [(Text, UUID, OrderStatus)])
getOrdersInTransit statuses pool = fmap (first (pack . show)) $ runTransaction pool Hasql.Read $ statuses `Hasql.statement` getOrdersInTransitStatement

getOrdersInTransitStatement :: Hasql.Statement [OrderStatus] [(Text, UUID, OrderStatus)]
getOrdersInTransitStatement =
  dimap (V.fromList . map encodeToText) (fromRight mkError . sequence . map convert . V.toList) $
  [TH.vectorStatement|
    SELECT
      id :: text,
      sdek_request_uuid :: uuid,
      to_jsonb(CAST(status AS text)) :: jsonb
    FROM orders
    WHERE 
      sdek_request_uuid IS NOT NULL
      AND
      status = ANY ($1 :: text[] :: order_status[])
  |]
  where convert (orderId, uuid, jsonStatus) = fmap (orderId, uuid,) $ convertFromJson @OrderStatus jsonStatus
        mkError = error "aeson decode failed on order status"
