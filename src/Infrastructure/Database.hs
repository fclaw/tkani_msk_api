-- We need TemplateHaskell to generate functions from SQL files
{-# LANGUAGE TemplateHaskell       #-}
-- Also helpful for writing the TH splices
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ScopedTypeVariables   #-}

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
  , markOrderAsInvalid
  , fetchCatalogSummaryItem
  , checkFabricPreCuts
  , insertNewPaymentRecord
  , fetchPendingPayments
  , updatePaymentStatusStatement
  , updatePaymentStatus
  , searchFabrics
  , searchFabricCard
  , saveDailyDigestDraft
  , checkDailyDigestDraft
  , updateDailyDigestDraft
  , setDailyDigestStatus
  , fetchPaymentId
  , module Types
  ) where


import qualified Hasql.Pool as Hasql
import qualified Hasql.Transaction as Hasql
import qualified Hasql.Transaction.Sessions as Hasql
import qualified Hasql.Statement as Hasql
import qualified Hasql.TH as Hasql
import Data.Profunctor.Unsafe (dimap, lmap, rmap)
import Data.Aeson (FromJSON, fromJSON, Result (..), Value, fromJSON, Result)
import Data.Text (Text, pack)
import Data.Bifunctor (first, second)
import Control.Monad (join, void)
import Data.Tuple.Ops (initT, app2, app3, app6, app7, snocT)
import Data.Int (Int64, Int32)
import Data.Maybe (fromMaybe)
import Data.UUID (UUID)
import qualified Data.Vector as V
import Data.Either (fromRight, either)
import Data.Time (Day)


import API.Types -- Your data types
import TH.RecordToTuple (recordToTuple)
import API.WithField (WithField)
import qualified Infrastructure.Database.Types as Types
import Infrastructure.Database.Types as Types
import Text (encodeToText)
import Infrastructure.Database.Fabric (ingestFabricDB)
import qualified Domain.Warehouse.Types as DWT
import Infrastructure.Services.Tinkoff.Types.GetState (GetStateRequest)
import Infrastructure.Services.Tinkoff.Types.GetState (Status (PENDING))

--------------------------------------------------------------------------------
-- Template Haskell Magic: Generate our statement functions automatically
--------------------------------------------------------------------------------

convertFromJson :: forall a . FromJSON a => Value -> Either String a
convertFromJson value =
  case fromJSON @a value of
   Success val -> Right val
   Error msg -> Left msg


runTransaction :: Hasql.Pool -> Hasql.Mode -> Hasql.Transaction a -> IO (Either Hasql.UsageError a)
runTransaction pool mode = Hasql.use pool . Hasql.transaction Hasql.Serializable mode

-- | Statement to fetch a single fabric row by its ID.
--   TH.singletonStatement reads the SQL, infers the parameter and result types.
getFabricStatement :: Hasql.Statement (Int64, Double) (Either String FullFabric)
getFabricStatement =
  dimap (first fromIntegral) (maybe (Left "fabric not found") id . fmap (convertFromJson @FullFabric))
  [Hasql.maybeStatement|
    SELECT jsonb_build_object(
        'id', f.id,
        'description', f.name,
        'total_length_m', CAST(f.total_length_m AS int4),
        'price_per_meter', f.price_per_meter,
        'available_length_m', f.available_length_m,
        'is_sold', f.is_sold,
        'article', f.article,
        'pre_cuts', pc_data.json_val,
        'warehouse_message_id', f.warehouse_message_id,
        'media_type', to_jsonb(f.media_type) :: jsonb
    ) :: jsonb
    FROM fabrics AS f
    CROSS JOIN LATERAL (
        SELECT coalesce(
            jsonb_agg(
                jsonb_build_object(
                    'id', pc.id,
                    'length_m', pc.length_m,
                    'price_rub', pc.price_rub,
                    'in_stock', pc.in_stock
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
          f.total_length_m > 0 AND
          f.available_length_m > 0.1 AND
          f.available_length_m < $2 :: float8 AND
          jsonb_array_length(pc_data.json_val) > 0
        )

        OR

        (
          CAST(f.available_length_m AS int4) = 0 AND
          CAST(f.total_length_m AS int4) = 0 AND
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


putNewFabric :: DWT.Fabric -> RawIngestRequest -> Hasql.Pool -> IO (Either Text Int64)
putNewFabric fabric req pool = 
  fmap (first (pack . show)) $ 
    runTransaction pool Hasql.Write $
      ingestFabricDB fabric req


getFinalOrderItemPriceStatement :: Hasql.Statement (Int64, Maybe Int64, Maybe Double) Double
getFinalOrderItemPriceStatement = 
  [Hasql.singletonStatement|
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
  [Hasql.singletonStatement|
    WITH inserted_order AS (
      INSERT INTO orders (
       id,
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
      RETURNING id
    )
    INSERT INTO order_fabric_bindings (
        order_id, 
        fabric_id,
        length_m,
        pre_cut_id
    ) 
    SELECT
        id, 
        $2 :: int8, 
        $3 :: float8?,
        $4 :: int8? 
    FROM inserted_order
    RETURNING order_id :: text
  |]

placeNewOrder :: Order -> Hasql.Pool -> IO (Either Text ())
placeNewOrder order pool = fmap (first (pack . show)) $ runTransaction pool Hasql.Write $ order `Hasql.statement` placeNewOrderStatement

setTelegramMessageStatement :: Hasql.Statement SetTelegramMessageRequest Int64
setTelegramMessageStatement =
   lmap (app3 fromIntegral . $(recordToTuple ''SetTelegramMessageRequest))
   [Hasql.rowsAffectedStatement| 
     INSERT INTO order_telegram_bindings 
     (order_id, chat_id, message_id) 
     VALUES ($1 :: text, $2 :: int8, $3 :: int4) |]

setTelegramMessage :: SetTelegramMessageRequest -> Hasql.Pool -> IO (Either Text Int64)
setTelegramMessage message pool = fmap (first (pack . show)) $ runTransaction pool Hasql.Write $ message `Hasql.statement` setTelegramMessageStatement

getChatDetailsStatement :: Hasql.Statement Text (Maybe Int)
getChatDetailsStatement = rmap (fmap fromIntegral) [Hasql.maybeStatement| SELECT message_id :: int FROM order_telegram_bindings WHERE order_id = $1 :: text |]

getChatDetails :: Text -> Hasql.Pool -> IO (Either Text (Maybe Int))
getChatDetails orderId pool = fmap (first (pack . show)) $ runTransaction pool Hasql.Read $ orderId `Hasql.statement` getChatDetailsStatement


updateOrderStatusStatement :: Hasql.Statement (Text, OrderStatus) Int
updateOrderStatusStatement = 
  dimap (second statusToSQL) fromIntegral
  [Hasql.singletonStatement| 
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
  [Hasql.singletonStatement|
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
            
            'rem_length', f.available_length_m :: float8,

            'warehouse_message_id', f.warehouse_message_id
        ) :: jsonb
  |]

fetchOrderStatus :: Text -> Hasql.Pool -> IO (Either Text (Maybe (OrderStatus, Text, Text, Providers)))
fetchOrderStatus query pool = fmap (join . first (pack . show)) $ runTransaction pool Hasql.Read $ query `Hasql.statement` fetchOrderStatusStatement


fetchOrderStatusStatement :: Hasql.Statement Text (Either Text (Maybe (OrderStatus, Text, Text, Providers)))
fetchOrderStatusStatement =
  rmap (sequence . fmap (first pack) . fmap convert)
  [Hasql.maybeStatement|
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
  [Hasql.vectorStatement|
    SELECT
      id :: text,
      sdek_request_uuid :: uuid,
      to_jsonb(CAST(status AS text)) :: jsonb
    FROM orders
    WHERE
      sdek_request_uuid IS NOT NULL
      AND
      is_removed_from_delivery_provider = FALSE
      AND
      status = ANY ($1 :: text[] :: order_status[])
  |]
  where convert (orderId, uuid, jsonStatus) = fmap (orderId, uuid,) $ convertFromJson @OrderStatus jsonStatus
        mkError = error "aeson decode failed on order status"

markOrderAsInvalid :: Text -> UUID -> Hasql.Pool -> IO (Either Text (Int, Text))
markOrderAsInvalid orderId uuid pool = 
  fmap (first (pack . show)) $ 
  runTransaction pool Hasql.Write $
    (orderId, uuid) `Hasql.statement` markOrderAsInvalidStatement

markOrderAsInvalidStatement :: Hasql.Statement (Text, UUID) (Int, Text)
markOrderAsInvalidStatement =
  rmap (first fromIntegral)
  [Hasql.singletonStatement| 
    UPDATE orders
    SET is_removed_from_delivery_provider = TRUE
    WHERE id = $1 :: text AND sdek_request_uuid = $2 :: uuid
    RETURNING internal_notification_message_id :: int8, sdek_tracking_number :: text
  |]


type SearchResultRow = (Int64, Value) -- (total_count, teaser_json)

-- A pure helper function for the transformation logic
processSearchResults :: [SearchResultRow] -> Either Text (Int, [SearchTeaser])
processSearchResults [] = Right (0, []) -- Handle the case of no results
processSearchResults allRows@((firstRowTotal, _):_) =
  let
      -- 1. Get the total count from the first row (it's the same in all rows)
      total = fromIntegral firstRowTotal
      -- 2. Map over all rows to decode the JSON blob into a SearchTeaser
      teasers = first pack $ sequence $ map (convertFromJson @SearchTeaser . snd) allRows -- 'snd' gets the ByteString part
  in fmap (total,) teasers


-- 1. Full Text Search (Smart matching)
-- 2. Fallback for strict matches (e.g. searching partial Article ID)
-- Rank results by relevance (Name match > Description match)
searchFabricsStatement :: Hasql.Statement (Text, Int32, Int32) (Either Text (Int, [SearchTeaser]))
searchFabricsStatement =
  rmap (processSearchResults . V.toList)
  [Hasql.vectorStatement|
    SELECT
      sfp.total_count :: int8,
      sfp.teaser_json :: jsonb
    FROM search_fabrics_paginated($1 :: text, $2 :: int4, $3 :: int4) AS sfp
  |]

searchFabrics :: Text -> Int -> Int -> Hasql.Pool -> IO (Either Text (Int, [SearchTeaser]))
searchFabrics query limit offset pool = 
  fmap (join . first (pack . show)) $ 
    runTransaction pool Hasql.Read $ 
      (query, fromIntegral limit, fromIntegral offset) `Hasql.statement` searchFabricsStatement

fetchCatalogSummaryItemStatement :: Hasql.Statement (Day, Double) [CatalogSummaryItem]
fetchCatalogSummaryItemStatement =
  rmap (V.toList . V.map (either error id . convertFromJson)) $
  [Hasql.vectorStatement|
    SELECT item_json :: jsonb
      FROM (
          SELECT
              f.updated_at,
              jsonb_build_object(
                  'id', f.id,
                  'name', f.name,
                  'article', f.article,
                  'type', 'roll',
                  'price_per_meter', f.price_per_meter,
                  'total_price', NULL,
                  'length_m', NULL,
                  'available_length', f.available_length_m,
                  'is_sold_out', f.is_sold,
                  'warehouse_message_id', f.warehouse_message_id,
                  'warehouse_chat_id', -1001234567890,
                  'warehouse_file_id', f.image_url,
                  'description', f.description,
                  'media_type', to_jsonb(f.media_type),
                  'width', f.width
              ) :: jsonb AS item_json
          FROM 
              fabrics AS f
          WHERE
              CAST(f.updated_at AS date) = $1 :: date
              AND f.is_sold = FALSE
              AND f.available_length_m > $2 :: float8

          UNION ALL

          SELECT
              f.updated_at,
              jsonb_build_object(
                  'id', f.id,
                  'name', f.name || ' (отрез ' || pc.length_m || 'м)',
                  'article', f.article,
                  'type', 'pre_cut',
                  'price_per_meter', NULL,
                  'total_price', pc.price_rub,
                  'length_m', pc.length_m,
                  'is_sold_out', FALSE,
                  'warehouse_message_id', f.warehouse_message_id,
                  'warehouse_chat_id', -1001234567890,
                  'warehouse_file_id', f.image_url,
                  'description', f.description,
                  'media_type', to_jsonb(f.media_type),
                  'width', f.width
              ) :: jsonb AS item_json
          FROM 
              pre_cuts AS pc
          JOIN 
              fabrics AS f ON pc.fabric_id = f.id
          WHERE
              CAST(f.updated_at AS date) = $1 :: date
              AND pc.in_stock = TRUE
      ) AS catalog_items
    ORDER BY updated_at DESC
  |]

fetchCatalogSummaryItem :: Day -> Double -> Hasql.Pool -> IO (Either Text [CatalogSummaryItem])
fetchCatalogSummaryItem day threshold pool = 
  fmap (first (pack . show)) $ 
    runTransaction pool Hasql.Write $ 
      (day, threshold) `Hasql.statement` fetchCatalogSummaryItemStatement


checkFabricPreCutsStatement :: Hasql.Statement Text Bool
checkFabricPreCutsStatement =
  [Hasql.singletonStatement|
    SELECT EXISTS (
      SELECT 1 
      FROM fabrics f
      JOIN pre_cuts pc ON f.id = pc.fabric_id
      WHERE f.article = $1 :: text
      AND pc.in_stock = TRUE
    ) :: bool
  |]

checkFabricPreCuts :: Text -> Hasql.Pool -> IO (Either Text Bool)
checkFabricPreCuts articleId pool = fmap (first (pack . show)) $ runTransaction pool Hasql.Read $ articleId `Hasql.statement` checkFabricPreCutsStatement

insertNewPaymentRecordStatement :: Hasql.Statement NewPaymentRecord Int64
insertNewPaymentRecordStatement =
  dimap (app2 encodeToText . $(recordToTuple ''NewPaymentRecord)) fromIntegral
  [Hasql.singletonStatement|
    INSERT INTO payments (
      order_id,
      provider,
      provider_payment_id,
      amount,
      payment_url, 
      error,
      token
    ) VALUES (
      $1 :: text,
      cast($2 :: text as payment_provider),
      $3 :: text,
      $4 :: int8,
      $5 :: text,
      $6 :: text?,
      $7 :: text
    )
    RETURNING id :: int8
  |]

insertNewPaymentRecord :: NewPaymentRecord -> Hasql.Pool -> IO (Either Text Int64)
insertNewPaymentRecord paymentRecord pool = 
  fmap (first (pack . show)) $ 
    runTransaction pool Hasql.Write $
      paymentRecord `Hasql.statement` 
      insertNewPaymentRecordStatement


fetchPendingPaymentsStatement :: Hasql.Statement Status [(Text, Text)]
fetchPendingPaymentsStatement =
  dimap encodeToText V.toList $
  [Hasql.vectorStatement|
    SELECT
      order_id :: text,
      provider_payment_id::text
    FROM payments
    WHERE status = CAST(LOWER($1 :: text) as payment_status)
  |]

fetchPendingPayments :: Hasql.Pool -> IO (Either Text [(Text, Text)])
fetchPendingPayments pool = fmap (first (pack . show)) $ runTransaction pool Hasql.Read $ PENDING `Hasql.statement` fetchPendingPaymentsStatement

updatePaymentStatusStatement :: Hasql.Statement (Text, Status, Status) Int64
updatePaymentStatusStatement = 
  dimap (app3 encodeToText . app2 encodeToText) fromIntegral $
  [Hasql.rowsAffectedStatement|
    UPDATE payments
    SET status = CAST(LOWER($2 :: text) as payment_status)
    WHERE 
      status = CAST(LOWER($3 :: text) as payment_status) 
    AND
      order_id = $1 :: text
  |]

updatePaymentStatus :: Text -> Status -> OrderStatus -> Hasql.Pool -> IO (Either Text Int)
updatePaymentStatus orderId paymentStatus orderStatus pool = 
  fmap (first (pack . show)) $ 
    runTransaction pool Hasql.Write $ do
      void $ (orderId, paymentStatus, PENDING) `Hasql.statement` updatePaymentStatusStatement
      (orderId, orderStatus) `Hasql.statement` updateOrderStatusStatement


searchFabricCardStatement :: Hasql.Statement (DWT.FabricType, Int64) (Maybe CatalogSummaryItem)
searchFabricCardStatement = 
  dimap (first encodeToText) (fmap (fromRight undefined . convertFromJson))
  [Hasql.maybeStatement|
    WITH item AS (
        SELECT
          jsonb_build_object(
            'id', f.id,
            'name', f.name,
            'article', f.article,
            'type', 'roll',
            'price_per_meter', f.price_per_meter,
            'total_price', NULL,
            'length_m', NULL,
            'available_length', f.available_length_m,
            'is_sold_out', f.is_sold,
            'warehouse_message_id', f.warehouse_message_id,
            'warehouse_chat_id', -1001234567890,
            'warehouse_file_id', f.image_url,
            'description', f.description,
            'media_type', to_jsonb(f.media_type),
            'width', f.width
              ) :: jsonb AS item_json
        FROM fabrics AS f
        WHERE $1 :: text = 'roll' AND f.id = $2 :: int8
      UNION ALL
        SELECT
          jsonb_build_object(
            'id', f.id,
            'name', f.name || ' (отрез ' || pc.length_m || 'м)',
            'article', f.article,
            'type', 'pre_cut',
            'price_per_meter', NULL,
            'total_price', pc.price_rub,
            'length_m', pc.length_m,
            'is_sold_out', FALSE,
            'warehouse_message_id', f.warehouse_message_id,
            'warehouse_chat_id', -1001234567890,
            'warehouse_file_id', f.image_url,
            'description', f.description,
            'media_type', to_jsonb(f.media_type),
            'width', f.width
          ) :: jsonb AS item_json
        FROM pre_cuts AS pc
        JOIN fabrics AS f ON pc.fabric_id = f.id
        WHERE $1 :: text = 'pre_cut' AND pc.id = $2 :: int8
    )
    SELECT item_json :: jsonb FROM item
  |]

searchFabricCard :: DWT.FabricType -> Int64 -> Hasql.Pool -> IO (Either Text (Maybe CatalogSummaryItem))
searchFabricCard fabricType fabricId pool =
  fmap (first (pack . show)) $ 
    runTransaction pool Hasql.Read $ 
      (fabricType, fabricId) `Hasql.statement` 
        searchFabricCardStatement


checkDailyDigestStatement :: Hasql.Statement Day Bool
checkDailyDigestStatement = [Hasql.singletonStatement|SELECT EXISTS(SELECT 1 FROM daily_digests WHERE announcement_date = $1 :: date) :: bool|]

checkDailyDigestDraft :: Day -> Hasql.Pool -> IO (Either Text Bool)
checkDailyDigestDraft day pool = 
  fmap (first (pack . show)) $ 
    runTransaction pool Hasql.Read $ 
      day `Hasql.statement` checkDailyDigestStatement

saveDailyDigestStatement :: Hasql.Statement (Day, Int64, Int64) ()
saveDailyDigestStatement =
  rmap (const ())
  [Hasql.rowsAffectedStatement|
    INSERT INTO daily_digests 
    ( announcement_date
    , warehouse_chat_id
    , warehouse_message_id)
    VALUES ($1 :: date, $2 :: int8, $3 :: int8)
    ON CONFLICT (announcement_date)
    DO NOTHING
  |]

saveDailyDigestDraft :: Day -> Int64 -> Int64 -> Hasql.Pool -> IO (Either Text ())
saveDailyDigestDraft day chatId messageId pool = 
  fmap (first (pack . show)) $ 
    runTransaction pool Hasql.Write $ 
      (day, chatId, messageId) `Hasql.statement` 
      saveDailyDigestStatement

updateDailyDigestStatement :: Hasql.Statement DailyDigestDraft ()
updateDailyDigestStatement =
  dimap $(recordToTuple ''DailyDigestDraft) (const ())
  [Hasql.rowsAffectedStatement|
    UPDATE daily_digests
    SET final_draft = $3 :: text
    WHERE warehouse_chat_id = $1 :: int8 
    AND warehouse_message_id = $2 :: int8
  |]


updateDailyDigestDraft :: DailyDigestDraft -> Hasql.Pool -> IO (Either Text ())
updateDailyDigestDraft draft pool =
  fmap (first (pack . show)) $ 
    runTransaction pool Hasql.Write $ 
      draft `Hasql.statement` 
      updateDailyDigestStatement

setDailyDigestStatusStatement :: DailyDigestStatus -> Hasql.Statement DailyDigest ()
setDailyDigestStatusStatement status = 
  dimap (snocT (encodeToText status) . $(recordToTuple ''DailyDigest)) (const ())
  [Hasql.rowsAffectedStatement|
    UPDATE daily_digests
    SET status = CAST($3 :: text AS daily_digests_status)
    WHERE warehouse_chat_id = $1 :: int8 
    AND warehouse_message_id = $2 :: int8
  |]

setDailyDigestStatus :: DailyDigest -> DailyDigestStatus -> Hasql.Pool -> IO (Either Text ())
setDailyDigestStatus publish status pool = 
  fmap (first (pack . show)) $ 
    runTransaction pool Hasql.Write $ 
      publish `Hasql.statement` (setDailyDigestStatusStatement status)


fetchPaymentIdStatement :: Hasql.Statement Text (Maybe Text)
fetchPaymentIdStatement = 
  [Hasql.maybeStatement|
    SELECT provider_payment_id :: text
    FROM payments 
    WHERE order_id = $1 :: text
  |]

fetchPaymentId :: Text -> Hasql.Pool -> IO (Either Text (Maybe Text))
fetchPaymentId order pool = fmap (first (pack . show)) $ runTransaction pool Hasql.Read $ order `Hasql.statement` fetchPaymentIdStatement