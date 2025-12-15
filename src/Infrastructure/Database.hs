-- We need TemplateHaskell to generate functions from SQL files
{-# LANGUAGE TemplateHaskell       #-}
-- Also helpful for writing the TH splices
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RecordWildCards   #-}

module Infrastructure.Database
  ( getFabricPreview
  , putNewFabric
  , getOrderItems
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
  , isItemInCart
  , addToCart
  , clearCart
  , clearCartStatement
  , clearOldCarts
  , fetchCartItems
  , getOrderItemsForAdjustStatement
  , patchRoll
  , patchPrecut
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
import Data.Tuple.Ops (initT, app1, app2, app3, app6, app7, snocT, app4, app5)
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
import Domain.Warehouse.Types (FabricType)

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
getFabricPreviewStatement :: Hasql.Statement (Int64, FabricType, Double) FabricPreview
getFabricPreviewStatement =
  dimap (app1 fromIntegral . app2 encodeToText) (either error id . convertFromJson @FabricPreview)
  [Hasql.singletonStatement|
    WITH claimed_length AS (
      SELECT 
        ci.fabric_id, 
        SUM(ci.length_m) AS length
      FROM cart_items ci
      WHERE ci.fabric_id = $1 :: int8
      AND ci.pre_cut_id IS NULL
      GROUP BY ci.fabric_id

      UNION ALL

      SELECT
        ofb.fabric_id,
        COALESCE(SUM(ofb.length_m), 0.0) AS length
      FROM order_fabric_bindings ofb
      JOIN orders o 
      ON ofb.order_id = o.id
      WHERE ofb.fabric_id = $1 :: int8
      AND ofb.pre_cut_id IS NULL
      AND o.status = 'registered'
      AND o.created_at > NOW() - INTERVAL '30 minutes'
      GROUP BY ofb.fabric_id
    ),
    pre_cut_in_order AS (
      SELECT 1 AS in_order
      FROM order_fabric_bindings ofb
      JOIN orders o
      ON ofb.order_id = o.id
      WHERE ofb.pre_cut_id = $1 :: int8
      AND o.status = 'registered'
      AND o.created_at > 
          NOW() - INTERVAL '30 minutes'
    )
    SELECT
      jsonb_build_object(
        'name', f.name :: text,
        'price', f.price_per_meter :: int4,
        'stock_available', 
          (f.available_length_m - 
           COALESCE(cl.length, 0.0)) :: float8,
        'status', 
          CASE
            WHEN (cl.fabric_id IS NOT NULL AND 
                  $3 :: float8 <= (
                    f.available_length_m - 
                    COALESCE(cl.length, 0.0))) OR
                 (cl.fabric_id IS NULL AND 
                  f.available_length_m >= $3 :: float8)
            THEN 'item_in_stock'
            WHEN (cl.fabric_id IS NULL AND 
                  f.available_length_m < $3 :: float8)
            THEN 'item_sold_out'
            ELSE 'item_is_claimed'
          END
      ) :: jsonb
    FROM fabrics AS f
    LEFT JOIN claimed_length AS cl
    ON f.id = cl.fabric_id
    WHERE f.id = $1 :: int8 AND $2 :: text = 'roll'

    UNION ALL

    SELECT
      jsonb_build_object(
        'name', f.name || ' (отрез ' || pc.length_m || 'м)' :: text,
        'price', pc.price_rub :: int4,
        'stock_available', pc.length_m,
        'status',
          CASE 
            WHEN ci.pre_cut_id IS NULL AND
                 pc.in_stock IS TRUE AND 
                 NOT EXISTS (
                   SELECT in_order 
                   FROM pre_cut_in_order)
            THEN 'item_in_stock'
            WHEN ci.pre_cut_id IS NULL AND
                 pc.in_stock IS FALSE AND
                 NOT EXISTS (
                   SELECT in_order 
                   FROM pre_cut_in_order)
            THEN 'item_sold_out'
            ELSE 'item_is_claimed'
          END
      ) :: jsonb
    FROM pre_cuts AS pc
    JOIN fabrics AS f
    ON pc.fabric_id = f.id
    LEFT JOIN cart_items as ci
    ON pc.id = ci.pre_cut_id
    WHERE pc.id = $1 :: int8 AND $2 :: text = 'pre_cut'
  |]


-- | Fetches a fabric and all its associated, in-stock pre-cuts from the database.
getFabricPreview :: Int64 -> FabricType -> Double -> Hasql.Pool -> IO (Either Text FabricPreview)
getFabricPreview fabricId fabricType threshold pool = 
  fmap (first (pack . show)) $
    runTransaction pool Hasql.Read $
      (fabricId, fabricType, threshold) `Hasql.statement` getFabricPreviewStatement

putNewFabric :: DWT.Fabric -> RawIngestRequest -> Hasql.Pool -> IO (Either Text (Int64, Text))
putNewFabric fabric req pool = 
  fmap (first (pack . show)) $ 
    runTransaction pool Hasql.Write $
      ingestFabricDB fabric req


getOrderItemsStatement :: Hasql.Statement Int64 [OrderItem]
getOrderItemsStatement =
  rmap (either error id . sequence . map (convertFromJson @OrderItem) . V.toList)
  [Hasql.vectorStatement|
    SELECT
      jsonb_build_object(
        'name', f.name,
        'article', f.article,
        'total_price', f.price_per_meter * ci.length_m,
        'fabric_type', ci.item_type,
        'price_per_metre', f.price_per_meter,
        'length_m', ci.length_m,
        'telegram_url', ci.telegram_url
       ) :: jsonb
     FROM carts AS c
     INNER JOIN cart_items AS ci
     ON c.id = ci.cart_id
     INNER JOIN fabrics AS f
     ON f.id = ci.fabric_id
     WHERE c.telegram_user_id = $1 :: int8

    UNION ALL

    SELECT
      jsonb_build_object(
        'name', f.name,
        'article', f.article,
        'total_price', pc.price_rub,
        'fabric_type', ci.item_type,
        'price_per_metre', null,
        'length_m', null,
        'telegram_url', ci.telegram_url
      ) :: jsonb
    FROM carts AS c
    INNER JOIN cart_items AS ci
    ON c.id = ci.cart_id
    INNER JOIN pre_cuts AS pc
    ON pc.id = ci.pre_cut_id
    INNER JOIN fabrics AS f
    ON f.id = pc.fabric_id
    WHERE c.telegram_user_id = $1 :: int8

  |]

-- | Fetches the final, calculated price for a fabric order item.
--   The entire calculation (per-meter vs. fixed price) is handled by the SQL query.
--   Returns 'Nothing' if the fabric or pre-cut is not found.
getOrderItems :: Int64 -> Hasql.Pool -> IO (Either Text [OrderItem])
getOrderItems userId pool = 
  fmap (first (pack . show)) $
    runTransaction pool Hasql.Write $ 
      userId `Hasql.statement` getOrderItemsStatement

placeNewOrderStatement :: Hasql.Statement Order ()
placeNewOrderStatement = 
  lmap $(recordToTuple ''Order)
  [Hasql.resultlessStatement|
    WITH inserted_order AS (
      INSERT INTO orders (
       id,
       customer_full_name,
       customer_phone,
       delivery_provider_id,
       delivery_point_id,
       sdek_request_uuid,
       sdek_tracking_number,
       internal_notification_message_id,
       created_at,
       updated_at,
       status
      ) VALUES (
       $1 :: text,
       $2 :: text,
       $3 :: text,
       $4 :: text,
       $5 :: text,
       $6 :: uuid,
       $7 :: text,
       $8 :: int8,
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
      (SELECT id FROM inserted_order),
      COALESCE(ci.fabric_id, pc.fabric_id),
      ci.length_m,
      ci.pre_cut_id 
    FROM carts AS c
    INNER JOIN cart_items AS ci
    ON c.id = ci.cart_id
    LEFT JOIN pre_cuts AS pc
    ON pc.id = ci.pre_cut_id
    WHERE c.telegram_user_id = $9 :: int8 
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
adjustFabric :: Hasql.Statement (Int64, Maybe Int64, Maybe Double, Double) (Result AdjustFabric)
adjustFabric =
  rmap (fromJSON @AdjustFabric)
  [Hasql.singletonStatement|
    WITH update_pc AS (
        UPDATE pre_cuts 
        SET in_stock = FALSE
        WHERE pre_cuts.id = $2 :: int8?
    )

    UPDATE fabrics f
    SET 
        available_length_m = CASE
            WHEN $2 :: int8? IS NULL 
            THEN f.available_length_m - $3 :: float8?
            ELSE f.available_length_m
        END,

        is_sold = CASE 
            WHEN $2 :: int8? IS NULL THEN 
              (f.available_length_m - $3 :: float8?) <= 0.01
            
            ELSE NOT EXISTS (
                SELECT 1 FROM pre_cuts pc 
                WHERE pc.fabric_id = $1 :: int8
                  AND pc.in_stock = TRUE 
                  AND pc.id <> $2 :: int8?
            )
        END
    WHERE f.id = $1 :: int8

    RETURNING
        jsonb_build_object(
            'name', f.description :: text,
            'article', f.article :: text,
            'is_sold', f.is_sold :: bool,
            
            'is_pre_cut_req', (
                $2 :: int8? IS NULL AND 
                f.available_length_m > 0.01 AND 
                f.available_length_m < $4 :: float8
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
searchFabricsStatement :: Hasql.Statement (Text, Int32, Int32, Double) (Either Text (Int, [SearchTeaser]))
searchFabricsStatement =
  rmap (processSearchResults . V.toList)
  [Hasql.vectorStatement|
    SELECT
      sfp.total_count :: int8,
      sfp.teaser_json :: jsonb
    FROM search_fabrics_paginated($1 :: text, $2 :: int4, $3 :: int4, $4 :: float8) AS sfp
  |]

searchFabrics :: Text -> Int -> Int -> Double -> Hasql.Pool -> IO (Either Text (Int, [SearchTeaser]))
searchFabrics query limit offset metreThreshold pool = 
  fmap (join . first (pack . show)) $ 
    runTransaction pool Hasql.Read $ 
      Hasql.statement 
      ( query, 
        fromIntegral limit, 
        fromIntegral offset, 
        metreThreshold) 
      searchFabricsStatement

fetchCatalogSummaryItemStatement :: Hasql.Statement (Day, Double) [CatalogSummaryItem]
fetchCatalogSummaryItemStatement =
  rmap (V.toList . V.map (either error id . convertFromJson)) $
  [Hasql.vectorStatement|
    WITH pre_cut_in_order AS (
      SELECT ofb.pre_cut_id as pre_cut_id
      FROM order_fabric_bindings ofb
      JOIN orders o
      ON ofb.order_id = o.id
      WHERE ofb.pre_cut_id IS NOT NULL
      AND o.status = 'registered'
      AND o.created_at > 
          NOW() - INTERVAL '30 minutes'
    )
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
              'available_length', 
                f.available_length_m - 
                COALESCE(locked_stock.total_locked, 0),
              'is_sold_out', f.is_sold,
              'warehouse_message_id', f.warehouse_message_id,
              'warehouse_chat_id', -1001234567890,
              'warehouse_file_id', f.image_url,
              'description', f.description,
              'media_type', to_jsonb(f.media_type),
              'width', f.width
            ) AS item_json
          FROM 
            fabrics AS f
          LEFT JOIN (
            SELECT 
              ci.fabric_id, 
              SUM(ci.length_m) AS total_locked
            FROM cart_items ci
            JOIN carts c 
            ON ci.cart_id = c.id
            WHERE 
              ci.item_type = 'roll'
              AND c.updated_at > NOW() - INTERVAL '30 minutes' 
            GROUP BY ci.fabric_id

            UNION ALL

            SELECT
              ofb.fabric_id,
              COALESCE(SUM(ofb.length_m), 0.0) AS total_locked
            FROM order_fabric_bindings ofb
            JOIN orders o 
            ON ofb.order_id = o.id
            WHERE 
              ofb.fabric_id IS NOT NULL
              AND ofb.pre_cut_id IS NULL
              AND o.status = 'registered'
              AND o.created_at > NOW() - INTERVAL '30 minutes'
            GROUP BY ofb.fabric_id 
          ) AS locked_stock
          ON f.id = locked_stock.fabric_id
          WHERE
            CAST(f.updated_at AS date) = $1 :: date
            AND f.is_sold = FALSE
            AND (f.available_length_m - COALESCE(locked_stock.total_locked, 0)) > $2 :: float8

          UNION ALL

          SELECT
              f.updated_at,
              jsonb_build_object(
                  'id', pc.id,
                  'name', f.name || ' (отрез ' || pc.length_m || 'м)',
                  'article', f.article,
                  'type', 'pre_cut',
                  'price_per_meter', NULL,
                  'total_price', pc.price_rub,
                  'length_m', pc.length_m,
                  'available_length', null,
                  'is_sold_out', FALSE,
                  'warehouse_message_id', f.warehouse_message_id,
                  'warehouse_chat_id', -1001234567890,
                  'warehouse_file_id', f.image_url,
                  'description', f.description,
                  'media_type', to_jsonb(f.media_type),
                  'width', f.width
              ) :: jsonb AS item_json
          FROM pre_cuts AS pc
          LEFT JOIN cart_items AS ci
          ON pc.id = ci.pre_cut_id
          JOIN fabrics AS f ON pc.fabric_id = f.id
          LEFT JOIN pre_cut_in_order as pcio
          ON pcio.pre_cut_id = ci.id
          WHERE CAST(f.updated_at AS date) = $1 :: date
          AND pc.in_stock = TRUE
          AND ci.pre_cut_id IS NULL
          AND pcio.pre_cut_id IS NULL
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


searchFabricCardStatement :: Hasql.Statement (DWT.FabricType, Int64, Double) (Maybe CatalogSummaryItem)
searchFabricCardStatement = 
  dimap (app1 encodeToText) (fmap (fromRight undefined . convertFromJson))
  [Hasql.maybeStatement|
    WITH claimed_length AS (
      SELECT 
        ci.fabric_id, 
        SUM(ci.length_m) AS length
      FROM cart_items ci
      WHERE ci.fabric_id = $2 :: int8
      AND ci.pre_cut_id IS NULL
      GROUP BY ci.fabric_id

      UNION ALL

      SELECT
        ofb.fabric_id,
        COALESCE(SUM(ofb.length_m), 0.0) AS length
      FROM order_fabric_bindings ofb
      JOIN orders o 
      ON ofb.order_id = o.id
      WHERE ofb.fabric_id = $2 :: int8
      AND ofb.pre_cut_id IS NULL
      AND o.status = 'registered'
      AND o.created_at > NOW() - INTERVAL '30 minutes'
      GROUP BY ofb.fabric_id
    ),
    item AS (
        SELECT
          jsonb_build_object(
            'id', f.id,
            'name', f.name,
            'article', f.article,
            'type', 'roll',
            'price_per_meter', f.price_per_meter,
            'total_price', NULL,
            'length_m', NULL,
            'available_length', 
              (f.available_length_m - 
               COALESCE(cl.length, 0.0)) :: float8,
            'is_sold_out', f.is_sold,
            'warehouse_message_id', f.warehouse_message_id,
            'warehouse_chat_id', -1001234567890,
            'warehouse_file_id', f.image_url,
            'description', f.description,
            'media_type', to_jsonb(f.media_type),
            'width', f.width
              ) :: jsonb AS item_json
        FROM fabrics AS f
        LEFT JOIN claimed_length as cl
        ON cl.fabric_id = f.id
        WHERE $1 :: text = 'roll' 
        AND f.id = $2 :: int8
        AND COALESCE(cl.length, 0.0) >= $3 :: float8 
      UNION ALL
        SELECT
          jsonb_build_object(
            'id', pc.id,
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
        LEFT JOIN cart_items AS ci
        ON pc.id = ci.pre_cut_id
        JOIN fabrics AS f ON pc.fabric_id = f.id
        WHERE $1 :: text = 'pre_cut' 
        AND pc.id = $2 :: int8
        AND ci.pre_cut_id IS NULL 
        AND NOT EXISTS (
          SELECT 1
          FROM order_fabric_bindings ofb
          JOIN orders o 
          ON ofb.order_id = o.id
          WHERE ofb.pre_cut_id = $2 :: int8
          AND o.status = 'registered'
          AND o.created_at >
              NOW() - INTERVAL '30 minutes')
    )
    SELECT item_json :: jsonb FROM item
  |]

searchFabricCard :: DWT.FabricType -> Int64 -> Double -> Hasql.Pool -> IO (Either Text (Maybe CatalogSummaryItem))
searchFabricCard fabricType fabricId threshold pool =
  fmap (first (pack . show)) $ 
    runTransaction pool Hasql.Read $ 
      (fabricType, fabricId, threshold) `Hasql.statement` 
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


isItemInCartStatement :: Hasql.Statement (Int64, FabricType, Int64) CartCheckStatus
isItemInCartStatement =
  dimap (app2 encodeToText) (either error id . convertFromJson @CartCheckStatus)
  [Hasql.singletonStatement|
    SELECT 
      to_jsonb((CASE
        WHEN c.id IS NULL THEN 'no_cart_exists'
        WHEN c.updated_at <= NOW() - INTERVAL '30 minutes' THEN 'cart_expired'
        WHEN EXISTS (
            SELECT 1 
            FROM cart_items ci
            WHERE ci.cart_id = c.id AND
                CASE
                    WHEN $2 :: text = 'pre_cut' THEN 
                      ci.item_type = 'pre_cut' AND 
                      ci.pre_cut_id = $3 :: int8
                    WHEN $2 :: text = 'roll' THEN 
                      ci.item_type = 'roll' AND 
                      ci.fabric_id = $3 :: int8
                    ELSE FALSE
                END
        ) THEN 'item_in_cart'
        ELSE 'ok_to_add'
      END) :: text) :: jsonb
    FROM
    (SELECT 1) AS dummy
    LEFT JOIN
      carts AS c ON c.telegram_user_id = $1 :: int8
  |]

isItemInCart :: Int64 -> FabricType -> Int64 ->  Hasql.Pool -> IO (Either Text CartCheckStatus)
isItemInCart userId fabricType fabricId pool = 
  fmap (first (pack . show)) $ 
    runTransaction pool Hasql.Read $ 
      (userId, fabricType, fabricId) `Hasql.statement` isItemInCartStatement

addToCartStatement :: Hasql.Statement CartNewFabric ()
addToCartStatement = 
  dimap (app4 encodeToText . $(recordToTuple ''CartNewFabric)) (const ())
  [Hasql.singletonStatement|
     WITH user_cart AS (
       SELECT id
       FROM carts
       WHERE telegram_user_id = $1 :: int8 
       AND updated_at > NOW() - INTERVAL '30 minutes'
      ),
     created_cart AS (
       INSERT INTO carts 
       (telegram_user_id)
       (SELECT $1 :: int8
       WHERE NOT EXISTS (SELECT 1 FROM user_cart))
       ON CONFLICT (telegram_user_id)
       DO UPDATE SET updated_at = NOW()
       RETURNING id
     ),
     target_cart AS (
       SELECT id FROM user_cart
       UNION ALL
       SELECT id FROM created_cart
     ),
     locked_pre_cut AS (
        UPDATE pre_cuts 
        SET in_stock = false
        WHERE 
          CAST($4 :: text AS cart_item_type) = 'pre_cut' 
          AND id = $2 :: int8
     )
     INSERT INTO cart_items
     ( cart_id
     , item_type
     , fabric_id
     , pre_cut_id
     , length_m
     , telegram_url)
     SELECT
      (SELECT id FROM target_cart),
      CAST($4 :: text AS cart_item_type),
      CASE WHEN CAST($4 :: text AS cart_item_type) = 'roll'
           THEN $2 :: int8
           ELSE null 
      END,
      CASE WHEN CAST($4 :: text AS cart_item_type) = 'pre_cut' 
           THEN $2 :: int8
           ELSE null
      END,
      $3 :: float8?,
      $5 :: text
      RETURNING id :: int8
  |]


-- CRITICAL: Lock the parent fabric row for the duration of the transaction.
-- This prevents any other transaction from modifying its length or adding
-- another piece to a cart until this transaction is committed or rolled back.
isRollAvailableStatement :: Hasql.Statement (Int64, Maybe Double, Int) Bool
isRollAvailableStatement =
  lmap (app3 fromIntegral) $
  [Hasql.singletonStatement|
    WITH locked_stock AS (
        SELECT 
          COALESCE(SUM(length_m), 0.0) AS total
        FROM cart_items
        WHERE 
          fabric_id = $1 :: int8 
          AND pre_cut_id IS NULL

        UNION ALL

        SELECT 
          COALESCE(SUM(ofb.length_m), 0.0) AS total
        FROM order_fabric_bindings ofb
        JOIN orders o 
        ON ofb.order_id = o.id
        WHERE 
          ofb.fabric_id = $1 :: int8
          AND ofb.pre_cut_id IS NULL
          AND o.status = 'registered'
          AND o.created_at > NOW() - INTERVAL '30 minutes'
    ),   
    total_claimed AS (
      SELECT SUM(total) as length
      FROM locked_stock
    )
    SELECT
      ((f.available_length_m - total_claimed.length) >= 
       (COALESCE($2 :: float8?, 0.0) + $3 :: float8)) :: bool
    FROM fabrics f, total_claimed
    WHERE f.id = $1 :: int8
    FOR UPDATE
  |]

-- CRITICAL: Lock this specific pre_cut row.
-- If another user tries to add the same pre_cut, their transaction will
-- wait for this lock. Since we will be updating 'in_stock' to FALSE if
-- we proceed, their check will then correctly fail.
isPreCutAvailableStatement :: Hasql.Statement Int64 Bool
isPreCutAvailableStatement =
  [Hasql.singletonStatement|
     SELECT
       ( pc.in_stock AND
         NOT EXISTS (
           SELECT 1
           FROM order_fabric_bindings ofb
           JOIN orders o 
           ON ofb.order_id = o.id
           WHERE ofb.pre_cut_id = pc.id
           AND o.status = 'registered'
           AND o.created_at > 
               NOW() - INTERVAL '30 minutes')
        ) :: bool
     FROM pre_cuts AS pc
     WHERE id = $1 :: int8
     FOR UPDATE
  |]

addToCart :: CartNewFabric -> Int -> Hasql.Pool -> IO (Either Hasql.UsageError CartCheckStatus)
addToCart item@CartNewFabric{..} cutTolerance pool = 
  runTransaction pool Hasql.Write $ do

    -- 1.  Run the appropriate availability check inside the transaction
    isAvailable <-
      case cnfFabricType of
        DWT.Roll -> 
          Hasql.statement 
          (cnfFabricId, 
           cnfFabricLength, 
           cutTolerance) 
          isRollAvailableStatement
        DWT.PreCut -> 
          Hasql.statement 
          cnfFabricId 
          isPreCutAvailableStatement

    -- 2. Branch on the result
    if isAvailable
    then do
           -- If available, insert the item into the cart
           Hasql.statement item addToCartStatement
           -- Return a success status
           return OkToAdd
    else
        -- If not available, return a failure status without inserting
        return ItemIsAlreadyClaimed

clearCartStatement :: Hasql.Statement Int64 ()
clearCartStatement =
  [Hasql.resultlessStatement|
    WITH released_percuts AS (
      SELECT pre_cut_id
      FROM cart_items
      WHERE cart_id IN (
        SELECT id FROM carts
        WHERE telegram_user_id = $1 :: int8
      ) AND pre_cut_id IS NOT NULL
    ),
    cleared_cart AS (
      DELETE FROM carts
      WHERE telegram_user_id = $1 :: int8
    )
    UPDATE pre_cuts
    SET in_stock = TRUE
    WHERE id IN (
      SELECT pre_cut_id 
      FROM released_percuts
    )
  |]

clearCart :: Int64 -> Hasql.Pool -> IO (Either Text ())
clearCart userId pool = 
  fmap (first (pack . show)) $ 
    runTransaction pool Hasql.Write $
      Hasql.statement userId clearCartStatement

clearOldCarts :: Hasql.Pool -> IO (Either Text ())
clearOldCarts pool = 
  fmap (first (pack . show)) $ 
    runTransaction pool Hasql.Write $ 
      Hasql.statement () 
      [Hasql.resultlessStatement|
         WITH released_percuts AS (
           SELECT pre_cut_id
           FROM cart_items
           WHERE cart_id IN (
             SELECT id FROM carts
             WHERE 
               updated_at < 
               NOW() - INTERVAL '30 minutes'
           ) AND pre_cut_id IS NOT NULL
         ),
         cleared_cart AS (
           DELETE FROM carts
           WHERE 
             updated_at < 
             NOW() - INTERVAL '30 minutes'
         )
         UPDATE pre_cuts
         SET in_stock = TRUE
         WHERE id IN (
           SELECT pre_cut_id 
           FROM released_percuts
         )
      |]

fetchCartItemsStatement :: Hasql.Statement Int64 [ViewCartItem]
fetchCartItemsStatement =
  rmap (either error id . sequence . map (convertFromJson @ViewCartItem) . V.toList)
  [Hasql.vectorStatement|
     SELECT
       jsonb_build_object(
        'id', f.id,
        'name', f.name,
        'type', ci.item_type,
        'length_m', ci.length_m,
        'price', ci.length_m * f.price_per_meter
       ) :: jsonb
     FROM carts as c 
     INNER JOIN cart_items as ci
     ON c.id = ci.cart_id
     INNER JOIN fabrics as f
     ON ci.fabric_id = f.id
     WHERE c.telegram_user_id = $1 :: int8

     UNION ALL

     SELECT
       jsonb_build_object(
        'id', pc.id,
        'name', f.name,
        'type', ci.item_type,
        'length_m', pc.length_m,
        'price', pc.price_rub
       ) :: jsonb
     FROM cart_items as ci
	   INNER JOIN carts as c
     ON c.id = ci.cart_id
     INNER JOIN pre_cuts as pc
     ON pc.id = ci.pre_cut_id
	   INNER JOIN fabrics as f
     ON pc.fabric_id = f.id
     WHERE c.telegram_user_id = $1 :: int8
  |]

fetchCartItems :: Int64 -> Hasql.Pool -> IO (Either Text [ViewCartItem])
fetchCartItems userId pool = fmap (first (pack . show)) $ runTransaction pool Hasql.Read $ Hasql.statement userId fetchCartItemsStatement

getOrderItemsForAdjustStatement ::  Hasql.Statement Text [(Int64, Maybe Int64, Maybe Double)]
getOrderItemsForAdjustStatement =
  rmap V.toList
  [Hasql.vectorStatement|
    SELECT
      fabric_id :: int8,
      pre_cut_id :: int8?,
      length_m :: float8?
    FROM order_fabric_bindings
    WHERE order_id = $1 :: text
  |]


patchRoll :: PatchedFabric -> Hasql.Pool -> IO (Either Text ())
patchRoll fabric pool = 
  fmap (first (pack . show)) $ 
    runTransaction pool Hasql.Write $ 
      Hasql.statement fabric $
        lmap ( app4 fromIntegral 
             . app5 fromIntegral 
             . $(recordToTuple ''PatchedFabric))
        [Hasql.resultlessStatement|
          UPDATE fabrics 
          SET
            description = $2 :: text,
            total_length_m = $3 :: float8,
            available_length_m = $3 :: float8,
            width = $4 :: int4,
            price_per_meter = $5 :: int4,
            is_searchable = $6 :: bool,
            name = $7 :: text,
            image_url = $8 :: text?,
            media_group_id = $9 :: text?,
            thumbnail_url = $10 :: text?,
            media_type = $11 :: text
          WHERE id = $1 :: int8
        |]

patchPrecut :: PatchedFabric -> Hasql.Pool -> IO (Either Text ())
patchPrecut fabric pool =
  fmap (first (pack . show)) $ 
    runTransaction pool Hasql.Write $ 
      Hasql.statement fabric $
        lmap ( app4 fromIntegral
             . app5 fromIntegral
             . $(recordToTuple ''PatchedFabric))
        [Hasql.resultlessStatement|
          WITH new_precut AS (
            UPDATE pre_cuts
            SET
             length_m = $3 :: float8,
             price_rub = $5 :: int4,
             is_searchable = $6 :: bool
            WHERE id = $1 :: int8
            RETURNING fabric_id :: int8)
          UPDATE fabrics
          SET
            description = $2 :: text,
            width = $4 :: int4,
            name = $7 :: text,
            image_url = $8 :: text?,
            media_group_id = $9 :: text?,
            thumbnail_url = $10 :: text?,
            media_type = $11 :: text
          WHERE id = (SELECT * FROM new_precut)
        |]
