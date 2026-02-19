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
  ( -- re-export
    module Types
  , module Utils
    -- utils
  , extractADT  
  , getFabricPreview
  , putNewFabric
  , getOrderItems
  , placeNewOrder
  , setTelegramMessage
  , getChatDetails
  , updateOrderStatusStatement
  , updateShelfOrderStatusStatement
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
  , deleteFabric
  , fetchOrdersForCourierPickup
  , createCourierPickupPromise
  , markedOrderAsMeasured
   -- yaml order
  , placeNewYamlOrder
  , getYamlOrderDetailsForPricing
  , getOrderDetailsForPricing
  , getPatchedOrderDetails
  , setReceiptReady
  , refreshAndFetchDailyStats
  , fetchOrderDeliveryItem
  , insertTelegramOrderDeliveryPost
  , refreshAndFetchMonthlyStats
  , tallyUpExpenses
  , recordAndLinkPickup
  , fetchWeightTrackerStateInfo
  , getTodaysDostavistaOrder
  , setDostavistaOrderStatus
  , setDostavistaPickupByCourierStatus
  , setOrderDimensions
  , fetchDostavistaPackages
  , fetchSpecialPostDetails
  , insertNewSpecialPost
  , deleteSpecialPost
  , saveTemporaryNotificationMessage
  , sweepTemporaryNotificationMessages
    -- Shelf section
  , initShelf
  , InitShelf (..)
  , fetchShelfItems
  , getPutOnDShelfDetails
  , finalizeShelfCheckout
  , moveItemsToShelfStatement
  , setFirstItemAddedStatement
  , fetchShelfItemsForShipment
  , placeNewShelfOrder
  , getShelfStatus
  , saveShelfSubmissionInfo
  , getShelfPersonalInfo
  , editShelfPersonalInfo
    -- courier 
  , getAppStatusDetails
  , updatePickupAppStatus
  , updatePickedUpOrdersStatus
   -- fabric media
  , addMediaToFabric
  , fetchCancelledOrders
  , markedCancelledOrders
    -- emergency case when order fails to be patched
  , fetchOrderDetailsForYaml
  , fetchLostParcels
  , fetchPreferredSdekPoint
  , removePreferredSdekPoint
  ) where


import qualified Hasql.TH as Hasql
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import Data.Text (Text, pack)
import Data.Bifunctor (first, second, bimap)
import Data.Int (Int64, Int32)
import Data.Maybe (fromMaybe)
import Data.UUID (UUID)
import qualified Data.Vector as V
import Data.Either (fromRight, either)
import Data.Time (Day)
import Control.Applicative ((<|>))
import Data.Foldable (for_)
import Control.Exception (throwIO)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.FileEmbed (embedFile)
import qualified Data.Text.Encoding as TE
import qualified Hasql.Pool as Hasql
import qualified Hasql.Transaction as Hasql
import qualified Hasql.Transaction.Sessions as Hasql
import qualified Hasql.Statement as Hasql
import Data.Time.Calendar.Month (Month)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (join, void, forM_, when)
import Data.Profunctor.Unsafe (dimap, lmap, rmap)
import Data.Aeson (FromJSON, fromJSON, Result (..), Value, fromJSON, Result)
import Data.Tuple.Ops (initT, app1, app2, app3, app6, app7, consT, snocT, app4, app5, sel2, del9, del3, del7)


import App (AppM, PaymentFlow, ChatKey (..))
import Utils.Sql (splitSql)
import API.Types hiding (Active) -- Your data types
import TH.RecordToTuple (recordToTuple, tupleToRecord)
import API.WithField (WithField)
import qualified Infrastructure.Database.Types as Types
import Infrastructure.Database.Types as Types
import Text (encodeToText, tshow)
import Infrastructure.Services.Dostavista.Types (DostavistaPackage)
import Domain.Warehouse.Enums (FabricLifecycle)
import Infrastructure.Database.Fabric (ingestFabricDB)
import qualified Domain.Warehouse.Types as DWT
import Infrastructure.Services.Tinkoff.Types.GetState (GetStateRequest)
import Infrastructure.Services.Tinkoff.Types.GetState (Status (PENDING))
import Domain.Warehouse.Types (FabricType)
import Infrastructure.Database.Utils as Utils
import Infrastructure.Services.Sdek.Types.Courier (SdekPickupAppStatus)
import Infrastructure.Services.Dostavista.Types.Enums (DostavistaOrderStatus (..))

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
{-# INLINE runTransaction #-}

runTransactionM :: MonadIO m => Hasql.Pool -> Hasql.Mode -> Hasql.Transaction a -> m (Either Hasql.UsageError a)
runTransactionM pool mode = liftIO . Hasql.use pool . Hasql.transaction Hasql.Serializable mode
{-# INLINE runTransactionM #-}

extractADT = either error id
{-# INLINE extractADT #-}

execCmd cmd = Hasql.statement () $ Hasql.Statement (TE.encodeUtf8 cmd) HE.noParams HD.noResult False
{-# INLINE execCmd #-}

---- Statements ------

updateOrderStatusStatement :: Hasql.Statement (Text, OrderStatus) (Maybe Int64)
updateOrderStatusStatement = 
  lmap (second statusToSQL) $
  [Hasql.maybeStatement| 
    UPDATE orders 
    SET status = CAST($2 :: text AS order_status) 
    WHERE id = $1 :: text 
    RETURNING COALESCE(internal_notification_message_id, 0) :: int8
  |]

-- | Statement to fetch a single fabric row by its ID.
--   TH.singletonStatement reads the SQL, infers the parameter and result types.
getFabricPreviewStatement :: Hasql.Statement (Int64, FabricType, Double) FabricPreview
getFabricPreviewStatement =
  dimap (app1 fromIntegral . app2 encodeToText) (extractADT . convertFromJson @FabricPreview)
  [Hasql.singletonStatement|
    WITH all_claimed_pieces AS (
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

      UNION ALL

      SELECT
        soi.fabric_id,
        SUM(soi.length_m) AS length
      FROM shelf_order_items soi
      JOIN shelf_orders so
      ON soi.shelf_order_id = so.id
      WHERE soi.pre_cut_id IS NULL
      AND so.status = 'registered'
      AND so.created_at > NOW() - INTERVAL '30 minutes'
      GROUP BY soi.fabric_id
    ),
    total_claimed_length AS (
      SELECT
        fabric_id,
        SUM(length) as length
      FROM all_claimed_pieces
      GROUP BY fabric_id
    ),
    pre_cut_in_order AS (
      SELECT 1 AS in_order
      FROM order_fabric_bindings ofb
      JOIN orders o
      ON ofb.order_id = o.id
      WHERE ofb.pre_cut_id = $1 :: int8
      AND o.status = 'registered'
      AND o.created_at > NOW() - INTERVAL '30 minutes'

      UNION ALL

      SELECT 1
      FROM shelf_order_items soi
      JOIN shelf_orders so 
      ON soi.shelf_order_id = so.id
      WHERE soi.pre_cut_id = $1 :: int8
      AND so.status = 'registered'
      AND so.created_at > NOW() - INTERVAL '30 minutes'
    )
    SELECT
      jsonb_build_object(
        'name', f.name :: text,
        'price', ROUND(f.price_per_meter * (1 - f.discount)) :: int4,
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
    LEFT JOIN total_claimed_length AS cl
    ON f.id = cl.fabric_id
    WHERE f.id = $1 :: int8 AND $2 :: text = 'roll'

    UNION ALL

    SELECT
      jsonb_build_object(
        'name', f.name :: text,
        'price', ROUND(pc.price_rub * (1 - f.discount)),
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
getFabricPreview :: Int64 -> FabricType -> Double -> Hasql.Pool -> AppM (Either Text FabricPreview)
getFabricPreview fabricId fabricType threshold pool = 
  fmap (first (pack . show)) $
    runTransactionM pool Hasql.Read $
      (fabricId, fabricType, threshold) `Hasql.statement` getFabricPreviewStatement

putNewFabric :: DWT.Fabric -> RawIngestRequest -> Hasql.Pool -> AppM (Either Text (Int64, Text))
putNewFabric fabric req pool = 
  fmap (first (pack . show)) $ 
    runTransactionM pool Hasql.Write $
      ingestFabricDB fabric req


getOrderItemsStatement :: Hasql.Statement Int64 [OrderItem]
getOrderItemsStatement =
  rmap (extractADT . sequence . map (convertFromJson @OrderItem) . V.toList)
  [Hasql.vectorStatement|
    SELECT
      jsonb_build_object(
        'name', f.name,
        'article', f.article,
        'total_price', ROUND(f.price_per_meter * (1 - f.discount) * ci.length_m),
        'fabric_type', ci.item_type,
        'price_per_metre', ROUND(f.price_per_meter * (1 - f.discount)),
        'length_m', ci.length_m,
        'telegram_url', ci.telegram_url,
        'thumbnail_url', f.thumbnail_url
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
        'total_price', ROUND(pc.price_rub * (1 - f.discount)),
        'fabric_type', ci.item_type,
        'price_per_metre', null,
        'length_m', null,
        'telegram_url', ci.telegram_url,
        'thumbnail_url', f.thumbnail_url
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
getOrderItems :: Int64 -> Hasql.Pool -> AppM (Either Text [OrderItem])
getOrderItems userId pool = 
  fmap (first (pack . show)) $
    runTransactionM pool Hasql.Write $ 
      userId `Hasql.statement` getOrderItemsStatement

placeNewOrderStatement :: Hasql.Statement Order Int64
placeNewOrderStatement = 
  lmap $(recordToTuple ''Order)
  [Hasql.rowsAffectedStatement|
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
       tariff,
       created_at,
       updated_at,
       status,
       is_bot,
       actual_weight_grams
      ) VALUES (
       $1 :: text,
       $2 :: text,
       $3 :: text,
       $4 :: text,
       $5 :: text,
       $6 :: uuid,
       $7 :: text,
       $8 :: int8,
       $10 :: int4,
       now(),
       now(),
       'registered',
       true,
       COALESCE(
        (SELECT
         SUM(ROUND(
          COALESCE(ci.length_m, pc.length_m) *
          COALESCE(f.weight_per_metre, pc_parent_fabric.weight_per_metre)
         ))
        FROM carts AS c
        INNER JOIN cart_items AS ci 
        ON c.id = ci.cart_id
        LEFT JOIN fabrics AS f 
        ON ci.fabric_id = f.id
        LEFT JOIN pre_cuts AS pc 
        ON ci.pre_cut_id = pc.id
        LEFT JOIN fabrics AS pc_parent_fabric 
        ON pc.fabric_id = pc_parent_fabric.id
        WHERE c.telegram_user_id = $9 :: int8)
        , 0))
      RETURNING id)
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

placeNewOrder :: Order -> Hasql.Pool -> AppM (Either Text Int64)
placeNewOrder order pool = fmap (first (pack . show)) $ runTransactionM pool Hasql.Write $ order `Hasql.statement` placeNewOrderStatement

setTelegramMessageStatement :: Hasql.Statement SetTelegramMessageRequest Int64
setTelegramMessageStatement =
   lmap $(recordToTuple ''SetTelegramMessageRequest)
   [Hasql.rowsAffectedStatement| 
     INSERT INTO order_telegram_bindings 
     (order_id, shelf_order_id, chat_id, message_id) 
     VALUES ($1 :: text?, $2 :: text?, $3 :: int8, $4 :: int8)|]

setTelegramMessage :: SetTelegramMessageRequest -> Hasql.Pool -> AppM (Either Text Int64)
setTelegramMessage message pool = fmap (first (pack . show)) $ runTransactionM pool Hasql.Write $ message `Hasql.statement` setTelegramMessageStatement

getChatDetailsStatement :: Hasql.Statement Text (Maybe (Int64, Int64))
getChatDetailsStatement = 
  [Hasql.maybeStatement| 
    SELECT 
      chat_id :: int8, 
      message_id :: int8
    FROM order_telegram_bindings 
    WHERE order_id = $1 :: text 
    OR shelf_order_id = $1 :: text |]

getChatDetails :: Text -> Hasql.Pool -> AppM (Either Text (Maybe (Int64, Int64)))
getChatDetails orderId pool = fmap (first (pack . show)) $ runTransactionM pool Hasql.Read $ orderId `Hasql.statement` getChatDetailsStatement


updateOrderStatus :: Text -> OrderStatus -> Maybe UTCTime -> Hasql.Pool -> AppM (Either Text Int64)
updateOrderStatus orderId status keepFreeUntil pool = 
  fmap (first (pack . show)) $ 
    runTransactionM pool Hasql.Write $ 
      Hasql.statement 
      (orderId, status, keepFreeUntil) $
        dimap (app2 statusToSQL) fromIntegral
        [Hasql.singletonStatement|
          UPDATE orders 
          SET status = CAST($2 :: text AS order_status),
          keep_free_until = $3 :: timestamptz?   
          WHERE id = $1 :: text
          RETURNING COALESCE(internal_notification_message_id, 0) :: int8
        |]

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

fetchOrderStatus :: Text -> Hasql.Pool -> AppM (Either Text (Maybe (OrderStatus, Text, Text, Providers)))
fetchOrderStatus query pool = fmap (join . first (pack . show)) $ runTransactionM pool Hasql.Read $ query `Hasql.statement` fetchOrderStatusStatement


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

getOrdersInTransit :: [OrderStatus] -> Hasql.Pool -> AppM (Either Text [(Text, UUID, OrderStatus)])
getOrdersInTransit statuses pool = fmap (first (pack . show)) $ runTransactionM pool Hasql.Read $ statuses `Hasql.statement` getOrdersInTransitStatement

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

markOrderAsInvalid :: Text -> UUID -> Hasql.Pool -> AppM (Either Text (Int64, Text))
markOrderAsInvalid orderId uuid pool = 
  fmap (first (pack . show)) $ 
  runTransactionM pool Hasql.Write $
    (orderId, uuid) `Hasql.statement` markOrderAsInvalidStatement

markOrderAsInvalidStatement :: Hasql.Statement (Text, UUID) (Int64, Text)
markOrderAsInvalidStatement =
  [Hasql.singletonStatement| 
    UPDATE orders
    SET is_removed_from_delivery_provider = TRUE
    WHERE id = $1 :: text AND sdek_request_uuid = $2 :: uuid
    RETURNING COALESCE(internal_notification_message_id, 0) :: int8, sdek_tracking_number :: text
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

searchFabrics :: Text -> Int -> Int -> Double -> Hasql.Pool -> AppM (Either Text (Int, [SearchTeaser]))
searchFabrics query limit offset metreThreshold pool = 
  fmap (join . first (pack . show)) $ 
    runTransactionM pool Hasql.Read $ 
      Hasql.statement 
      ( query, 
        fromIntegral limit, 
        fromIntegral offset, 
        metreThreshold) 
      searchFabricsStatement

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

checkFabricPreCuts :: Text -> Hasql.Pool -> AppM (Either Text Bool)
checkFabricPreCuts articleId pool = 
  fmap (first (pack . show)) $ 
    runTransactionM pool Hasql.Read $ 
      articleId `Hasql.statement` checkFabricPreCutsStatement

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
      token,
      payment_flow,
      shelf_order_id
    ) VALUES (
      $1 :: text?,
      cast($2 :: text as payment_provider),
      $3 :: text,
      $4 :: int8,
      $5 :: text,
      $6 :: text?,
      $7 :: text,
      CAST(LOWER($8 :: text) as payment_flow_types),
      $9 :: text?
    )
    RETURNING id :: int8
  |]

insertNewPaymentRecord :: NewPaymentRecord -> Hasql.Pool -> AppM (Either Text Int64)
insertNewPaymentRecord paymentRecord pool = 
  fmap (first (pack . show)) $ 
    runTransactionM pool Hasql.Write $
      paymentRecord `Hasql.statement` 
      insertNewPaymentRecordStatement


fetchPendingPaymentsStatement :: Hasql.Statement Status [(PaymentFlow, Maybe Text, Maybe Text, Text)]
fetchPendingPaymentsStatement =
  dimap encodeToText (map (app1 (extractADT . convertFromJson @PaymentFlow)) . V.toList) $
  [Hasql.vectorStatement|
    SELECT
      to_jsonb(CAST(payment_flow AS text)) :: jsonb,
      order_id :: text?,
      shelf_order_id :: text?,
      provider_payment_id::text
    FROM payments
    WHERE status = CAST(LOWER($1 :: text) as payment_status)
  |]

fetchPendingPayments :: Hasql.Pool -> AppM (Either Text [(PaymentFlow, Maybe Text, Maybe Text, Text)])
fetchPendingPayments pool = 
  fmap (first (pack . show)) $
    runTransactionM pool Hasql.Read $ 
      PENDING `Hasql.statement` fetchPendingPaymentsStatement

updatePaymentStatusStatement :: Hasql.Statement (Text, Status, Status) Int
updatePaymentStatusStatement = 
  dimap (app3 encodeToText . app2 encodeToText) fromIntegral $
  [Hasql.rowsAffectedStatement|
    UPDATE payments
    SET status = CAST(LOWER($2 :: text) as payment_status)
    WHERE status = CAST(LOWER($3 :: text) as payment_status) 
    AND (order_id = $1 :: text OR 
         shelf_order_id = $1 :: text)
  |]

updatePaymentStatus :: Text -> Status -> Hasql.Pool -> AppM (Either Text Int64)
updatePaymentStatus orderId paymentStatus pool = 
  fmap (first (pack . show)) $ 
    runTransactionM pool Hasql.Write $ do
      void $ (orderId, paymentStatus, PENDING) `Hasql.statement` updatePaymentStatusStatement
      maybeOrderMessageId <- 
        Hasql.statement (orderId, API.Types.Cancelled) $
          updateOrderStatusStatement
      
      maybeShelfOrderMessageId <- 
        Hasql.statement (orderId, Types.Cancelled) $
          updateShelfOrderStatusStatement
      
      return $ fromMaybe undefined $ maybeOrderMessageId <|> maybeShelfOrderMessageId


searchFabricCardStatement :: Hasql.Statement (DWT.FabricType, Int64, Double) (Maybe CatalogSummaryItem)
searchFabricCardStatement = 
  dimap (app1 encodeToText) (fmap (fromRight undefined . convertFromJson))
  [Hasql.maybeStatement|
    WITH all_claimed_pieces AS (
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

      UNION ALL

      SELECT
        soi.fabric_id,
        SUM(soi.length_m) AS length
      FROM shelf_order_items soi
      JOIN shelf_orders so
      ON soi.shelf_order_id = so.id
      WHERE soi.pre_cut_id IS NULL
      AND so.status = 'registered'
      AND so.created_at > NOW() - INTERVAL '30 minutes'
      GROUP BY soi.fabric_id
    ),
    total_claimed_length AS (
      SELECT
        fabric_id,
        SUM(length) as length
      FROM all_claimed_pieces
      GROUP BY fabric_id
    ),
    media_list AS (
      SELECT
       fabric_parent_id,
       fabric_type,
       array_agg(jsonb_build_object(
        'telegram_file_id', telegram_file_id,
        'media_type', media_type
       )) :: jsonb[] AS pictures
      FROM fabric_media
      GROUP BY fabric_parent_id, fabric_type
    ),
    item AS (
        SELECT
          jsonb_build_object(
            'id', f.id,
            'name', f.name,
            'article', f.article,
            'type', 'roll',
            'price_per_meter', ROUND(f.price_per_meter * (1 - f.discount)),
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
            'width', f.width,
            'media_list', COALESCE(ml.pictures, '{}' :: jsonb[])
              ) :: jsonb AS item_json
        FROM fabrics AS f
        LEFT JOIN total_claimed_length as cl
        ON cl.fabric_id = f.id
        LEFT JOIN media_list AS ml
        ON ml.fabric_parent_id = f.id 
        AND ml.fabric_type = 'roll'
        WHERE $1 :: text = 'roll' 
        AND f.id = $2 :: int8
        AND (f.available_length_m - COALESCE(cl.length, 0.0)) > $3 :: float8

      UNION ALL

        SELECT
          jsonb_build_object(
            'id', pc.id,
            'name', f.name,
            'article', f.article,
            'type', 'pre_cut',
            'price_per_meter', NULL,
            'total_price', ROUND(pc.price_rub * (1 - f.discount)),
            'length_m', pc.length_m,
            'is_sold_out', FALSE,
            'warehouse_message_id', f.warehouse_message_id,
            'warehouse_chat_id', -1001234567890,
            'warehouse_file_id', f.image_url,
            'description', f.description,
            'media_type', to_jsonb(f.media_type),
            'width', f.width,
            'media_list', COALESCE(ml.pictures, '{}' :: jsonb[])
          ) :: jsonb AS item_json
        FROM pre_cuts AS pc
        LEFT JOIN cart_items AS ci
        ON pc.id = ci.pre_cut_id
        JOIN fabrics AS f 
        ON pc.fabric_id = f.id
        LEFT JOIN media_list AS ml
        ON ml.fabric_parent_id = pc.id 
        AND ml.fabric_type = 'pre_cut'
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
              NOW() - INTERVAL '30 minutes'
              
          UNION
          
          SELECT 1
          FROM shelf_order_items soi
          JOIN shelf_orders so
          ON soi.shelf_order_id = so.id
          WHERE soi.pre_cut_id = $2 :: int8
          AND so.status = 'registered'
          AND so.created_at >
              NOW() - INTERVAL '30 minutes'
        )
    )
    SELECT item_json :: jsonb FROM item
  |]

searchFabricCard :: DWT.FabricType -> Int64 -> Double -> Hasql.Pool -> AppM (Either Text (Maybe CatalogSummaryItem))
searchFabricCard fabricType fabricId threshold pool =
  fmap (first (pack . show)) $ 
    runTransactionM pool Hasql.Read $ 
      (fabricType, fabricId, threshold) `Hasql.statement` 
        searchFabricCardStatement

fetchPaymentIdStatement :: Hasql.Statement Text (Maybe Text)
fetchPaymentIdStatement = 
  [Hasql.maybeStatement|
    SELECT provider_payment_id :: text
    FROM payments 
    WHERE order_id = $1 :: text
  |]

fetchPaymentId :: Text -> Hasql.Pool -> AppM (Either Text (Maybe Text))
fetchPaymentId order pool = fmap (first (pack . show)) $ runTransactionM pool Hasql.Read $ order `Hasql.statement` fetchPaymentIdStatement


isItemInCartStatement :: Hasql.Statement (Int64, FabricType, Int64) CartCheckStatus
isItemInCartStatement =
  dimap (app2 encodeToText) (extractADT . convertFromJson @CartCheckStatus)
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

isItemInCart :: Int64 -> FabricType -> Int64 ->  Hasql.Pool -> AppM (Either Text CartCheckStatus)
isItemInCart userId fabricType fabricId pool = 
  fmap (first (pack . show)) $ 
    runTransactionM pool Hasql.Read $ 
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
isRollAvailableStatement :: Hasql.Statement (Int64, Maybe Double, Double) Bool
isRollAvailableStatement =
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

        UNION ALL

        SELECT 
          COALESCE(SUM(soi.length_m), 0.0) AS total
        FROM shelf_order_items soi
        JOIN shelf_orders so
        ON soi.shelf_order_id = so.id
        WHERE 
          soi.fabric_id = $1 :: int8
          AND soi.pre_cut_id IS NULL
          AND so.status = 'registered'
          AND so.created_at > NOW() - INTERVAL '30 minutes'
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
       pc.in_stock :: bool
     FROM pre_cuts AS pc
     WHERE id = $1 :: int8
     AND NOT EXISTS (
      SELECT 1
      FROM order_fabric_bindings ofb
      JOIN orders o 
      ON ofb.order_id = o.id
      WHERE ofb.pre_cut_id = pc.id
      AND o.status = 'registered'
      AND o.created_at >
          NOW() - INTERVAL '30 minutes'

      UNION
      SELECT 1
      FROM shelf_order_items soi
      JOIN shelf_orders so
      ON soi.shelf_order_id = so.id
      WHERE soi.pre_cut_id = pc.id
      AND so.status = 'registered'
      AND so.created_at >
          NOW() - INTERVAL '30 minutes'
      )
     FOR UPDATE
  |]

addToCart :: CartNewFabric -> Double -> Hasql.Pool -> AppM (Either Hasql.UsageError CartCheckStatus)
addToCart item@CartNewFabric{..} cutTolerance pool = 
  runTransactionM pool Hasql.Write $ do

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

clearCart :: Int64 -> Hasql.Pool -> AppM (Either Text ())
clearCart userId pool = 
  fmap (first (pack . show)) $ 
    runTransactionM pool Hasql.Write $
      Hasql.statement userId clearCartStatement

clearOldCarts :: Hasql.Pool -> AppM (Either Text ())
clearOldCarts pool = 
  fmap (first (pack . show)) $ 
    runTransactionM pool Hasql.Write $ 
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
  rmap (extractADT. sequence . map (convertFromJson @ViewCartItem) . V.toList)
  [Hasql.vectorStatement|
     SELECT
       jsonb_build_object(
        'id', f.id,
        'name', f.name,
        'type', ci.item_type,
        'length_m', ci.length_m,
        'price', ROUND(ci.length_m * f.price_per_meter * (1 - f.discount))
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
        'price', ROUND(pc.price_rub * (1 - f.discount))
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

fetchCartItems :: Int64 -> Hasql.Pool -> AppM (Either Text [ViewCartItem])
fetchCartItems userId pool = fmap (first (pack . show)) $ runTransactionM pool Hasql.Read $ Hasql.statement userId fetchCartItemsStatement

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

    UNION ALL

    SELECT
    fabric_id :: int8,
    pre_cut_id :: int8?,
    length_m :: float8?
    FROM shelf_order_items AS soi
    INNER JOIN shelf_orders AS so
    ON soi.shelf_order_id = so.id
    WHERE so.order_id = $1 :: text
  |]


patchRoll :: PatchedFabric -> Hasql.Pool -> AppM (Either Text ())
patchRoll fabric pool = 
  fmap (first (pack . show)) $ 
    runTransactionM pool Hasql.Write $ 
      Hasql.statement fabric $
        lmap ($(recordToTuple ''PatchedFabric))
        [Hasql.resultlessStatement|
          UPDATE fabrics AS f_alias
          SET
            description = $2 :: text,
            total_length_m = $3 :: float8,
            available_length_m = 
              GREATEST(
               0.0, 
               ($3 :: float8) - 
               (f_alias.total_length_m - 
                f_alias.available_length_m)),
            width = $4 :: int4,
            price_per_meter = $5 :: int4,
            is_searchable = $6 :: bool,
            name = $7 :: text,
            image_url = $8 :: text?,
            media_group_id = $9 :: text?,
            thumbnail_url = $10 :: text?,
            media_type = $11 :: text,
            lifecycle = COALESCE(CAST($12 :: text? AS fabric_lifecycle), f_alias.lifecycle),
            discount = COALESCE($13 :: float8?, f_alias.discount),
            updated_at = now()
          WHERE f_alias.id = $1 :: int8
        |]

patchPrecut :: PatchedFabric -> Hasql.Pool -> AppM (Either Text ())
patchPrecut fabric pool =
  fmap (first (pack . show)) $ 
    runTransactionM pool Hasql.Write $ 
      Hasql.statement fabric $
        lmap ($(recordToTuple ''PatchedFabric))
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
            media_type = $11 :: text,
            lifecycle = COALESCE(CAST($12 :: text? AS fabric_lifecycle), lifecycle),
            discount = COALESCE($13 :: float8?, discount),
            updated_at = now()
          WHERE id = (SELECT * FROM new_precut)
        |]

deleteFabric :: Int64 -> FabricType -> Hasql.Pool -> AppM (Either Text ())
deleteFabric fabricId fabricType pool = 
  fmap (first (pack . show)) $ 
    runTransactionM pool Hasql.Write $
      Hasql.statement fabricId $
        rmap (const ()) statement
  where 
    statement = 
      case fabricType of
        DWT.Roll -> 
          [Hasql.rowsAffectedStatement|
            UPDATE fabrics
            SET
              in_stock = FALSE,
              is_sold = TRUE,
              is_searchable = FALSE,
              available_length_m = 0.0,
              total_length_m = 0.0
            WHERE id = $1 :: int8
          |]
        DWT.PreCut ->
          [Hasql.rowsAffectedStatement|
            WITH updated_precut AS (
              UPDATE pre_cuts
              SET 
                in_stock = FALSE,
                is_searchable = FALSE
              WHERE id = $1 :: int8
              RETURNING fabric_id, 
              id AS pre_cut_id_just_updated)
            UPDATE fabrics f
            SET
              in_stock = FALSE,
              is_sold = TRUE,
              is_searchable = FALSE
            FROM updated_precut up
            WHERE f.id = up.fabric_id
            AND NOT EXISTS (
              SELECT 1
              FROM pre_cuts pc
              WHERE pc.fabric_id = up.fabric_id 
              AND pc.in_stock = TRUE
              AND pc.id <> up.pre_cut_id_just_updated)
          |]


-- Statement takes () and returns a list of (orderId, sdekUuid)
fetchOrdersForCourierPickup :: Hasql.Pool -> AppM (Either Text [OrdersForCourierPickup])
fetchOrdersForCourierPickup pool =
  fmap (first (pack . show)) $ 
    runTransactionM pool Hasql.Read $ 
      Hasql.statement () $
       rmap (map (extractADT . convertFromJson @OrdersForCourierPickup) . V.toList) $
       [Hasql.vectorStatement|
        WITH items AS (
          SELECT
           o.id AS order_id,
           CASE
            WHEN pc.id IS NULL
            THEN f.article
            ELSE pcf.article
           END AS article,
           CASE
            WHEN pc.id IS NULL
            THEN f.name
            ELSE pcf.name
           END AS name, 
           ROUND(
            CASE 
             WHEN pc.id IS NULL
             THEN f.weight_per_metre * ofb.length_m
             ELSE pc.length_m * pcf.weight_per_metre
            END) AS weight
          FROM orders AS o
          INNER JOIN order_fabric_bindings AS ofb
          ON o.id = ofb.order_id
          LEFT JOIN fabrics AS f
          ON f.id = ofb.fabric_id
          LEFT JOIN pre_cuts AS pc
          ON ofb.pre_cut_id = pc.id
          LEFT JOIN fabrics AS pcf
          ON pc.fabric_id = pcf.id
          
          UNION ALL
          
          SELECT
           o.id AS order_id,
           CASE
            WHEN pc.id IS NULL
            THEN f.article
            ELSE pcf.article
           END AS article,
           CASE
            WHEN pc.id IS NULL
            THEN f.name
            ELSE pcf.name
           END AS name,
           ROUND(
            CASE 
             WHEN pc.id IS NULL
             THEN f.weight_per_metre * si.length_m
             ELSE pc.length_m * pcf.weight_per_metre
            END) AS weight
          FROM orders AS o
          INNER JOIN shelf_items AS si
          ON si.main_order_id = o.id
          LEFT JOIN fabrics AS f
          ON f.id = si.fabric_id
          LEFT JOIN pre_cuts AS pc
          ON si.pre_cut_id = pc.id
          LEFT JOIN fabrics AS pcf
          ON pc.fabric_id = pcf.id

          UNION ALL

          SELECT
           o.id AS order_id,
           moi.article AS article,
           moi.item_name AS name,
           moi.weight AS weight
          FROM orders AS o
          INNER JOIN manual_order_items AS moi
          ON o.id = moi.order_id
        )
        SELECT
         json_build_object(
          'order_id', o.id :: text,
          'weight', o.actual_weight_grams :: int4,
          'length', o.length :: int4,
          'width', o.width :: int4,
          'height', o.height :: int4,
          'items', array_agg(
            json_build_object(
            'article', i.article :: text,
            'name', i.name :: text,
            'weight', i.weight :: int4)) :: jsonb[]
         ) :: jsonb
        FROM orders AS o
        INNER JOIN items AS i
        ON o.id = i.order_id
        WHERE status = 'paid'
        AND receipt_ready = TRUE
        AND (
         SELECT COUNT(*) = 0
         FROM courier_pickups 
         WHERE pickup_date = (now() + INTERVAL '1 day')::date)
        GROUP BY o.id, o.actual_weight_grams, o.length, o.width, o.height
        ORDER BY o.created_at DESC
       |]

createCourierPickupsStatement :: Hasql.Statement (UUID, UUID, Text, Day) Int64
createCourierPickupsStatement =
  [Hasql.singletonStatement|
    INSERT INTO courier_pickups (sdek_uuid, app_uuid, app_status, pickup_date)
    VALUES ($1 :: uuid, $2 :: uuid, $3 :: text, $4 :: date)
    RETURNING id :: int8
  |]

updateOrdersWithPickupUuidStatement :: Hasql.Statement (Int64, OrderStatus, V.Vector Text) ()
updateOrdersWithPickupUuidStatement =
  lmap (app2 encodeToText) $
  [Hasql.resultlessStatement|
    UPDATE orders
    SET sdek_courier_pickup_id = $1 :: int8,
        status = CAST($2 :: text AS order_status)
    WHERE id = ANY($3 :: text[])
  |]

createCourierPickupPromise :: UUID -> UUID -> Text -> [Text] -> Day -> Hasql.Pool -> AppM (Either Text ())
createCourierPickupPromise order_uuid app_uuid status orders date pool = 
  fmap (first (pack . show)) $ 
    runTransactionM pool Hasql.Write $ do 
      -- STEP 1: Insert all new pickup records.
      -- This uses UNNEST to handle the vector of data.
      pickupId <- Hasql.statement (order_uuid, app_uuid, status, date) createCourierPickupsStatement
      -- STEP 2: Update all associated orders to link them to this pickup.
      Hasql.statement (pickupId, ScheduledForPickup, V.fromList orders) updateOrdersWithPickupUuidStatement


markedOrderAsMeasured :: Text -> Hasql.Pool -> AppM (Either Text Bool)
markedOrderAsMeasured trackingN pool =
  fmap (first (pack . show)) $ 
    runTransactionM pool Hasql.Write $ 
      Hasql.statement trackingN $
        rmap (> 0) $
        [Hasql.rowsAffectedStatement|
          UPDATE orders
          SET is_measured = TRUE,
              updated_at = NOW()
          WHERE sdek_tracking_number = $1 :: text|]


placeNewYamlOrder :: YamlOrder -> [YamlOrderItem] -> Hasql.Pool -> AppM (Either Text Text)
placeNewYamlOrder order items pool =
  fmap (first (pack . show)) $ 
    runTransactionM pool Hasql.Write $ do
      orderId <- Hasql.statement order $
        lmap ($(recordToTuple ''YamlOrder))
        [Hasql.singletonStatement|
          INSERT INTO orders (
            id,
            customer_full_name,
            customer_phone,
            delivery_provider_id,
            delivery_point_id,
            sdek_request_uuid,
            sdek_tracking_number,
            tariff,
            actual_weight_grams,
            length,
            width,
            height,
            created_at,
            updated_at,
            status,
            is_bot
            ) VALUES (
            $1 :: text,
            $2 :: text,
            $3 :: text,
            $4 :: text,
            $5 :: text,
            $6 :: uuid,
            $7 :: text,
            $8 :: int4,
            $9 :: int4,
            $10 :: int4,
            $11 :: int4,
            $12 :: int4,
            now(),
            now(),
            'paid',
            false
            )
            RETURNING id :: text
        |]

      let params =
           snocT (V.fromList (map (\idx -> "ART-" <> tshow idx) [1 .. length items])) $
           consT orderId $ 
             V.unzip6 $ 
               V.fromList $ 
                 map (app6 fromIntegral .
                      app2 encodeToText .
                      $(recordToTuple ''YamlOrderItem)) 
                 items
      Hasql.statement params
        [Hasql.resultlessStatement|
          INSERT INTO manual_order_items (
            order_id,
            item_name,
            fabric_type,
            price_per_metre,
            total_price,
            length_m,
            weight,
            article
          )
          SELECT
            $1 :: text,
            items.name,
            items.fabric_type, 
            items.price_per_metre,
            items.total_price,
            items.length_m,
            items.weight,
            items.article
          FROM
          UNNEST(
            $2 :: text[],
            $3 :: text[], 
            $4 :: float8?[],
            $5 :: float8[],
            $6 :: float8?[],
            $7 :: int4[],
            $8 :: text[]
          ) AS items(name, fabric_type, price_per_metre, total_price, length_m, weight, article)
        |]

      return orderId  

getYamlOrderDetailsForPricing :: Text -> Hasql.Pool -> AppM (Either Text PriceInfo)
getYamlOrderDetailsForPricing orderId pool =
  fmap (first (pack . show)) $ 
    runTransactionM pool Hasql.Read $ 
      Hasql.statement orderId $
        rmap ( extractADT 
             . convertFromJson 
               @PriceInfo)
        [Hasql.singletonStatement|
          SELECT
            jsonb_build_object(
              'pick_up_point', TRIM(REGEXP_REPLACE(delivery_point_id, 'sdek_', '')),
              'weight', actual_weight_grams,
              'length', length,
              'width', width,
              'height', height,
              'tariff', tariff,
              'price', (
                SELECT SUM(CAST(total_price AS int)) 
                FROM manual_order_items
                WHERE order_id = $1 :: text)) :: jsonb
          FROM orders WHERE id = $1 :: text|]


getOrderDetailsForPricing :: Text -> Hasql.Pool -> AppM (Either Text PriceInfo)
getOrderDetailsForPricing orderId pool =
  fmap (first (pack . show)) $ 
    runTransactionM pool Hasql.Read $ 
      Hasql.statement orderId $
        rmap ( reducePriceInfoBot
             . extractADT
             . convertFromJson 
               @PriceInfoBot)
        [Hasql.singletonStatement|
          SELECT
            jsonb_build_object(
              'pick_up_point', r.pick_up_point,
              'tariff', r.tariff,
              'items', r.items) :: jsonb
          FROM    
          (SELECT 
            TRIM(REGEXP_REPLACE(o.delivery_point_id, 'sdek_', '')) AS pick_up_point,
            o.tariff,
            array_agg(
             jsonb_build_object(
              'density', f.density,
              'width', f.width,
              'length', COALESCE(ofb.length_m, pc.length_m),
              'weight_per_metre', f.weight_per_metre,
              'price',
                CASE
                 WHEN ofb.pre_cut_id IS NULL
                 THEN ROUND(ofb.length_m * f.price_per_meter * (1 - f.discount))
                 ELSE ROUND(pc.price_rub * (1 - f.discount))
                END          
              )) :: jsonb[] AS items
          FROM orders AS o
          INNER JOIN order_fabric_bindings AS ofb
          ON o.id = ofb.order_id
          LEFT JOIN fabrics AS f
          ON f.id = ofb.fabric_id
          LEFT JOIN pre_cuts AS pc
          ON ofb.pre_cut_id = pc.id
          WHERE o.id = $1 :: text
          GROUP BY o.delivery_point_id, o.tariff
          
          UNION ALL
          
          SELECT
            TRIM(REGEXP_REPLACE(o.delivery_point_id, 'sdek_', '')) AS pick_up_point,
            o.tariff,
            array_agg(
             jsonb_build_object(
              'density', f.density,
              'width', f.width,
              'length', COALESCE(si.length_m, pc.length_m),
              'weight_per_metre', f.weight_per_metre,
              'price',
               CASE
                 WHEN si.pre_cut_id IS NULL
                 THEN ROUND(si.length_m * f.price_per_meter * (1 - f.discount))
                 ELSE ROUND(pc.price_rub * (1 - f.discount))
                END 
              )) :: jsonb[] AS items
          FROM orders AS o
          INNER JOIN shelf_items AS si
          ON si.main_order_id = o.id
          LEFT JOIN fabrics AS f
          ON f.id = si.fabric_id
          LEFT JOIN pre_cuts AS pc
          ON si.pre_cut_id = pc.id
          WHERE o.id = $1 :: text
          GROUP BY o.delivery_point_id, o.tariff          
          ) AS r
        |]
        

getPatchedOrderDetails :: Text -> Hasql.Pool -> AppM (Either Text PatchedOrderDetails)
getPatchedOrderDetails orderId pool =
  fmap (first (pack . show)) $ 
    runTransactionM pool Hasql.Read $ 
      Hasql.statement orderId $
        rmap (extractADT . convertFromJson @PatchedOrderDetails)
        [Hasql.singletonStatement|
          WITH all_items_for_order AS (
           SELECT
           ofb.order_id,
           CASE
            WHEN ofb.length_m IS NOT NULL
            THEN f.name
            ELSE pcf.name 
           END AS name,
           CASE
            WHEN ofb.length_m IS NOT NULL
            THEN f.article
            ELSE pcf.article
            END AS article,
           CASE
            WHEN ofb.length_m IS NOT NULL
            THEN ROUND(f.weight_per_metre * ofb.length_m)
            ELSE ROUND(f.weight_per_metre * pc.length_m)
           END AS weight,
           CASE
            WHEN ofb.length_m IS NOT NULL
            THEN ROUND(f.price_per_meter * (1 - f.discount) * ofb.length_m)
            ELSE ROUND(pc.price_rub * (1 - f.discount))
           END AS total_price
          FROM order_fabric_bindings AS ofb
          LEFT JOIN fabrics AS f
          ON f.id = ofb.fabric_id
          LEFT JOIN pre_cuts AS pc
          ON ofb.pre_cut_id = pc.id
          LEFT JOIN fabrics AS pcf
          ON pc.fabric_id = pcf.id
          
          UNION ALL

          SELECT 
           si.main_order_id AS order_id,
           CASE
            WHEN si.length_m IS NOT NULL
            THEN f.name
            ELSE pcf.name
           END AS name,
           CASE
            WHEN si.length_m IS NOT NULL
            THEN f.article
            ELSE pcf.article
           END AS article,
           CASE
            WHEN si.length_m IS NOT NULL
            THEN ROUND(f.weight_per_metre * si.length_m)
            ELSE ROUND(f.weight_per_metre * pc.length_m)
           END AS weight,
           CASE
            WHEN si.length_m IS NOT NULL
            THEN ROUND(f.price_per_meter * (1 - f.discount) * si.length_m)
            ELSE ROUND(pc.price_rub * (1 - f.discount))
           END AS total_price
          FROM shelf_items AS si
          LEFT JOIN fabrics AS f
          ON f.id = si.fabric_id
          LEFT JOIN pre_cuts AS pc
          ON si.pre_cut_id = pc.id
          LEFT JOIN fabrics AS pcf
          ON pc.fabric_id = pcf.id 
          )
          SELECT 
            jsonb_build_object(
             'sdek_uuid', o.sdek_request_uuid,
             'items', array_agg(
               jsonb_build_object(
                'name', moi.item_name,
                'article', moi.article,
                'weight', moi.weight,
                'cost', moi.total_price
               ) ORDER BY moi.article)
            ) :: jsonb
          FROM orders AS o
          INNER JOIN manual_order_items AS moi
          ON o.id = moi.order_id 
          WHERE o.id = $1 :: text
          GROUP BY o.sdek_request_uuid
         
          UNION

          SELECT 
          jsonb_build_object(
           'sdek_uuid', o.sdek_request_uuid,
           'items', array_agg(
             jsonb_build_object(
              'name', items.name,
              'article', items.article,
              'weight', items.weight,
              'cost', items.total_price
             ) ORDER BY items.article)
          ) :: jsonb
          FROM orders AS o
          INNER JOIN all_items_for_order AS items
          ON o.id = items.order_id
          WHERE o.id = $1 :: text
          GROUP BY o.sdek_request_uuid
        |]

setReceiptReady :: Text -> UUID -> Hasql.Pool -> AppM (Either Text ())
setReceiptReady orderId uuid pool =
  fmap (first (pack . show)) $ 
    runTransactionM pool Hasql.Write $ 
      Hasql.statement (orderId, uuid) $
      [Hasql.resultlessStatement|
        UPDATE orders 
        SET receipt_ready = TRUE,
        receipt_uuid = $2 :: uuid
        WHERE id = $1 :: text|]


type DailyStatsRow = (Day, Int32, Double, Int32, Int32, Maybe Double, Either Text [DailyExpensesStat])

refreshAndFetchDailyStats :: Day -> Hasql.Pool -> AppM (Either Text [DailyStatsRow])
refreshAndFetchDailyStats day pool =
  fmap (first (pack . show)) $ 
    runTransactionM pool Hasql.Write $ do
      -- Execute the dynamic statement
      -- 1. Load the multi-command SQL file at compile time
      let multiCommandSql = TE.decodeUtf8 $(embedFile "sql/refresh_daily_stats.sql")
    
      -- 2. Split the text into a list of individual commands
      let commands = splitSql multiCommandSql

      -- 3. Execute each command as a separate, dynamic statement
      forM_ commands execCmd

      -- Step 2: Fetch the data for the last 30 days
      fmap (map (app7 ( first T.pack 
                      . sequence 
                      . map (convertFromJson @DailyExpensesStat)
                      . V.toList))
          . V.toList) $
        Hasql.statement day $
        [Hasql.vectorStatement|
          WITH daily_expenses_agg AS (
            SELECT
            expense_day,
            array_agg(
              jsonb_build_object(
                'payer', payer_name,
                'amount', total_amount,
                'transactions', transaction_count
            )) AS expenses_array
            FROM daily_expenses_summary
            GROUP BY expense_day
          ),
          combined_daily_stats AS (
            SELECT

            COALESCE(dss.sale_date, dea.expense_day) :: date AS report_date,

            COALESCE(dss.total_orders, 0) :: int4 AS total_orders,
            COALESCE(dss.total_revenue, 0.0) :: float8 AS total_revenue,
            COALESCE(dss.pre_cuts_sold_count, 0) :: int4 AS pre_cuts_sold_count,
            COALESCE(dss.rolls_sold_count, 0) :: int4 AS rolls_sold_count,
            COALESCE(dss.total_meters_sold, 0.0) :: float8? AS total_meters_sold,

            COALESCE(dea.expenses_array, array[]::jsonb[]) :: jsonb[] AS expenses

            FROM daily_sales_stats AS dss
            FULL OUTER JOIN daily_expenses_agg AS dea 
            ON dss.sale_date = dea.expense_day
            WHERE dss.sale_date IS NOT NULL 
            OR dea.expense_day IS NOT NULL)
          SELECT
            report_date :: date,
            total_orders :: int4,
            total_revenue :: float8,
            pre_cuts_sold_count :: int4,
            rolls_sold_count :: int4,
            total_meters_sold :: float8?,
            expenses :: jsonb[]
          FROM combined_daily_stats
          WHERE report_date >= $1 :: date
          ORDER BY report_date DESC LIMIT 30|]


fetchOrderDeliveryItem :: Day -> Hasql.Pool -> AppM (Either Text (Maybe Int32, [OrderDeliveryItem]))
fetchOrderDeliveryItem day pool =
  fmap (first (pack . show)) $ 
    runTransactionM pool Hasql.Read $
      Hasql.statement day $
        rmap (second (
            extractADT 
          . sequence 
          . map (convertFromJson @OrderDeliveryItem) 
          . V.toList)) $
        [Hasql.singletonStatement|
          SELECT
		      (SELECT
           odp.message_id
           FROM order_delivery_posts AS odp
           WHERE odp.created_at >= 
                 ($1 :: date - interval '1 day') 
           AND odp.created_at < $1 :: date
           ORDER BY odp.created_at DESC 
           LIMIT 1
          ) :: int?,
          COALESCE(array_agg(
           jsonb_build_object(
            'id', o.id,
            'track', o.sdek_tracking_number,
            'keep_free_until', o.keep_free_until
           ) ORDER BY o.created_at ASC), '{}'::jsonb[]) :: jsonb[]
          FROM orders AS o
		      WHERE o.status = 'delivered'
          AND NOT EXISTS (
	         SELECT 1
           FROM order_delivery_posts
           WHERE created_at :: date = now()::date)
		      LIMIT 20
        |]

insertTelegramOrderDeliveryPost :: Int32 -> Hasql.Pool -> AppM (Either Text ())
insertTelegramOrderDeliveryPost postId pool =
  fmap (first (pack . show)) $ 
    runTransactionM pool Hasql.Write $
      Hasql.statement postId $
      [Hasql.resultlessStatement|
        INSERT INTO order_delivery_posts
        (message_id) VALUES ($1 :: int)
      |]


type MonthlytatsRow = (Month, Int32, Int32, Double, Double)

refreshAndFetchMonthlyStats :: Hasql.Pool -> AppM (Either Text [MonthlyStat])
refreshAndFetchMonthlyStats pool = 
  fmap (first (pack . show)) $ 
    runTransactionM pool Hasql.Write $ do
      -- Execute the dynamic statement
      -- 1. Load the multi-command SQL file at compile time
      let multiCommandSql = TE.decodeUtf8 $(embedFile "sql/refresh_monthly_stats.sql")
    
      -- 2. Split the text into a list of individual commands
      let commands = splitSql multiCommandSql

      -- 3. Execute each command as a separate, dynamic statement
      forM_ commands execCmd

      fmap (map ($(tupleToRecord ''MonthlyStat) 
                 . app7 ( first T.pack 
                        . sequence 
                        . map (convertFromJson @DailyExpensesStat) 
                        . V.toList) 
                 . app1 ((read @Month) . T.unpack))
          . V.toList) $
        Hasql.statement () $
        [Hasql.vectorStatement|
          SELECT 
          sale_month :: text,
          total_monthly_orders :: int,
          average_orders_per_day :: int,
          total_estimated_profit :: float8,
          average_estimated_profit_per_day :: float8,

          total_monthly_expenses :: float8,
          expenses_by_payer :: jsonb[]

          FROM monthly_sales_stats AS mss
          JOIN monthly_expenses_summary AS mes
          ON mss.sale_month = mes.expense_month
          ORDER BY sale_month DESC LIMIT 12
        |]


tallyUpExpenses :: Expenses -> Hasql.Pool -> AppM (Either Text Bool)
tallyUpExpenses expenses pool =
  fmap (first (pack . show)) $ 
    runTransactionM pool Hasql.Write $
      Hasql.statement expenses $
      lmap $(recordToTuple ''Expenses)
      [Hasql.singletonStatement|
        SELECT create_expense($1 :: float8, $2 :: text?, $3 :: text?, $4 :: date?) :: bool
      |]

recordAndLinkPickup :: CourierPickupData -> Hasql.Pool -> AppM (Either Text ())
recordAndLinkPickup CourierPickupData {..} pool = 
 fmap (first (pack . show)) $ 
    runTransactionM pool Hasql.Write $ do
      Hasql.statement 
       ( cpdDay
       , encodeToText cpdProvider
       , cpdDostavistaOrderId
       , cpdCost
       , cpdDostavistaOrderStatus
       , V.fromList cpdOrders
       , cpdOrderStatus) $
         rmap (const ()) $
         [Hasql.rowsAffectedStatement|
          WITH new_pickup AS (
           INSERT INTO external_courier_pickups
           (pickup_date, provider, order_id, cost, status)
           VALUES 
           ( $1 :: date
           , CAST($2 :: text AS pickup_provider)
           , $3 :: int8
           , CAST($4 :: float8 AS numeric)
           , $5 :: text)
           ON CONFLICT (pickup_date) DO UPDATE
           SET provider = EXCLUDED.provider,
               order_id = EXCLUDED.order_id,
               cost = EXCLUDED.cost,
               status = EXCLUDED.status
           WHERE external_courier_pickups.status = 'canceled'
           RETURNING id :: int8)
          UPDATE orders
          SET courier_pickup_id = (SELECT id FROM new_pickup),
              status = CAST($7 :: text AS order_status)
          WHERE id = ANY($6 :: text[])
         |]


fetchWeightTrackerStateInfo :: Day -> CourierService -> Hasql.Pool -> AppM (Either Text (Int, Bool, [Text]))
fetchWeightTrackerStateInfo day service pool =
  fmap (first (pack . show)) $ 
    runTransactionM pool Hasql.Read $
      Hasql.statement (day, encodeToText service) $
      rmap (fromMaybe (0, False, []) . fmap (app1 fromIntegral . app3 V.toList))
      [Hasql.maybeStatement|
       SELECT
         COALESCE(SUM(o.actual_weight_grams), 0) :: int4,
         EXISTS (SELECT 1 FROM external_courier_pickups 
         WHERE provider = CAST($2 :: text AS pickup_provider) 
         AND pickup_date = $1 :: date 
         AND status != 'canceled') :: bool,
         COALESCE(array_agg(o.id), '{}'::text[]) :: text[]
       FROM orders AS o
       WHERE o.status = 'added_to_pickup_queue'
       AND o.receipt_ready = TRUE
       AND o.courier_pickup_id IS NULL
      |]

getTodaysDostavistaOrder :: Day -> Hasql.Pool -> AppM (Either Text (Maybe (Int64, DostavistaOrderStatus, UTCTime)))
getTodaysDostavistaOrder today pool =
  fmap (first (pack . show)) $
    runTransactionM pool Hasql.Read $
       Hasql.statement (today, V.fromList (map encodeToText [New, Available, Active])) $
       rmap (fmap (app2 (extractADT . convertFromJson @DostavistaOrderStatus)))
       [Hasql.maybeStatement|
         SELECT
         order_id :: int8, 
         to_jsonb(status) :: jsonb,
         created_at :: timestamptz
         FROM external_courier_pickups
         WHERE provider = 'dostavista'
         AND pickup_date = $1 :: date
         AND status = ANY($2 :: text[])
         AND now() - created_at < INTERVAL '2 hours'
       |]

setDostavistaOrderStatus :: Int64 -> DostavistaOrderStatus -> Hasql.Pool -> AppM (Either Text ())
setDostavistaOrderStatus orderId status pool =
  fmap (first (pack . show)) $
    runTransactionM pool Hasql.Write $ do
      ordersAffected <-
        Hasql.statement @_ @(V.Vector Text)
         (orderId, encodeToText status)
          [Hasql.singletonStatement|
           UPDATE external_courier_pickups
           SET status = $2 :: text
           WHERE order_id = $1 :: int8
           RETURNING (
            SELECT COALESCE(array_agg(id), '{}'::text[])
            FROM orders
            WHERE courier_pickup_id = 
                  external_courier_pickups.id
           ) :: text[]
          |]
       -- revert all orders linked to this pickup
      when(status == Canceled) $ do
        rowsAffected <- 
          Hasql.statement (ordersAffected) $
           [Hasql.rowsAffectedStatement|
            UPDATE orders
            SET courier_pickup_id = NULL,
                status = 'pickup_failed'
            WHERE id = ANY($1 :: text[])
           |]

        -- CRITICAL: Check that we actually updated one row.
        when (fromIntegral rowsAffected /= 
              length ordersAffected) $ do
          -- If not, something is wrong. Abort the transaction.
          error $ "Expected to update " <> 
                   show (length ordersAffected) <> 
                   " orders, but updated " <> 
                   show rowsAffected


      when(status == Infrastructure.Services.Dostavista.Types.Enums.Completed) $ do
        rowsAffected <- 
          Hasql.statement (ordersAffected) $
           [Hasql.rowsAffectedStatement|
            UPDATE orders
            SET status = 'on_route'
            WHERE id = ANY($1 :: text[])
          |]
        -- CRITICAL: Check that we actually updated one row.
        when (fromIntegral rowsAffected /= 
              length ordersAffected) $ do
          -- If not, something is wrong. Abort the transaction.
          error $ "Expected to update " <> 
                   show (length ordersAffected) <> 
                   " orders, but updated " <> 
                   show rowsAffected

setDostavistaPickupByCourierStatus :: Int64 -> DostavistaOrderStatus -> OrderStatus -> Hasql.Pool -> AppM (Either Text ())
setDostavistaPickupByCourierStatus orderId dostavistaStatus orderStatus pool =
  fmap (first (pack . show)) $
    runTransactionM pool Hasql.Write $
      Hasql.statement 
       ( orderId
       , encodeToText dostavistaStatus
       , encodeToText orderStatus)
       [Hasql.resultlessStatement|
        WITH pickup_id AS (
        UPDATE external_courier_pickups
        SET status = $2 :: text
        WHERE order_id = $1 :: int8
        RETURNING id :: int8)
        UPDATE orders 
        SET status = CAST($3 :: text AS order_status)
        WHERE courier_pickup_id = (SELECT * FROM pickup_id)
       |]

setOrderDimensions :: Text -> SetOrderDimensionsRequest -> Hasql.Pool -> AppM (Either Text ())
setOrderDimensions orderId dimensions pool =
  fmap (first (pack . show)) $
    runTransactionM pool Hasql.Write $
      Hasql.statement dimensions $
        lmap ( app3 fromIntegral 
             . app2 fromIntegral
             . app4 fromIntegral
             . consT orderId 
             . ($(recordToTuple ''SetOrderDimensionsRequest)))
        [Hasql.resultlessStatement|
         UPDATE orders
         SET
          length = $2 :: int4,
          width = $3 :: int4,
          height = $4 :: int4
         WHERE id = $1 :: text
       |]


fetchCatalogSummaryItem :: FabricLifecycle -> Int64 -> Double -> Hasql.Pool -> AppM (Either Text [CatalogSummaryItemExt])
fetchCatalogSummaryItem lifeCycle chatId threshold pool = 
  fmap (first (pack . show)) $ 
    runTransactionM pool Hasql.Read $ 
      Hasql.statement (encodeToText lifeCycle, chatId, threshold) $
        rmap (V.toList . V.map (extractADT . convertFromJson)) $
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

            UNION ALL
            
            SELECT soi.pre_cut_id as pre_cut_id
            FROM shelf_order_items soi
            JOIN shelf_orders so
            ON soi.shelf_order_id = so.id
            WHERE soi.pre_cut_id IS NOT NULL
            AND so.status = 'registered'
            AND so.created_at > 
                NOW() - INTERVAL '30 minutes'
          ),
          media_list AS (
            SELECT
            fabric_parent_id,
            fabric_type,
            array_agg(jsonb_build_object(
              'telegram_file_id', telegram_file_id,
              'media_type', media_type
            )) :: jsonb[] AS pictures
            FROM fabric_media
            GROUP BY fabric_parent_id, fabric_type
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
                    'price_per_meter', ROUND(f.price_per_meter * (1 - f.discount)),
                    'total_price', NULL,
                    'length_m', NULL,
                    'available_length', 
                      f.available_length_m - 
                      COALESCE(locked_stock.total_locked, 0),
                    'is_sold_out', f.is_sold,
                    'warehouse_message_id', f.warehouse_message_id,
                    'warehouse_chat_id', $2 :: int8,
                    'warehouse_file_id', f.image_url,
                    'description', f.description,
                    'media_type', to_jsonb(f.media_type),
                    'width', f.width,
                    'discount', f.discount,
                    'media_list', COALESCE(ml.pictures, '{}' :: jsonb[])
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

                  UNION ALL

                  SELECT
                    soi.fabric_id,
                    COALESCE(SUM(soi.length_m), 0.0) AS total_locked
                  FROM shelf_order_items soi
                  JOIN shelf_orders so 
                  ON soi.shelf_order_id = so.id
                  WHERE 
                    soi.fabric_id IS NOT NULL
                    AND soi.pre_cut_id IS NULL
                    AND so.status = 'registered'
                    AND so.created_at > NOW() - INTERVAL '30 minutes'
                  GROUP BY soi.fabric_id
                ) AS locked_stock
                ON f.id = locked_stock.fabric_id
                LEFT JOIN media_list AS ml
                ON f.id = ml.fabric_parent_id
                AND ml.fabric_type = 'roll'
                WHERE f.is_sold = FALSE
                AND (f.available_length_m - COALESCE(locked_stock.total_locked, 0.0)) > $3 :: float8
                AND f.lifecycle = CAST($1 :: text AS fabric_lifecycle)

                UNION ALL

                SELECT
                    f.updated_at,
                    jsonb_build_object(
                        'id', pc.id,
                        'name', f.name,
                        'article', f.article,
                        'type', 'pre_cut',
                        'price_per_meter', NULL,
                        'total_price', ROUND(pc.price_rub * (1 - f.discount)),
                        'length_m', pc.length_m,
                        'available_length', null,
                        'is_sold_out', FALSE,
                        'warehouse_message_id', f.warehouse_message_id,
                        'warehouse_chat_id', $2 :: int8,
                        'warehouse_file_id', f.image_url,
                        'description', f.description,
                        'media_type', to_jsonb(f.media_type),
                        'width', f.width,
                        'discount', f.discount,
                        'media_list', COALESCE(ml.pictures, '{}' :: jsonb[])
                    ) :: jsonb AS item_json
                FROM pre_cuts AS pc
                LEFT JOIN cart_items AS ci
                ON pc.id = ci.pre_cut_id
                JOIN fabrics AS f 
                ON pc.fabric_id = f.id
                LEFT JOIN pre_cut_in_order as pcio
                ON pcio.pre_cut_id = pc.id
                LEFT JOIN media_list AS ml
                ON pc.id = ml.fabric_parent_id
                AND ml.fabric_type = 'pre_cut'
                WHERE pc.in_stock = TRUE
                AND ci.pre_cut_id IS NULL
                AND pcio.pre_cut_id IS NULL
                AND f.lifecycle = CAST($1 :: text AS fabric_lifecycle)
            ) AS catalog_items
          ORDER BY updated_at DESC
        |]

fetchDostavistaPackages :: [Text] -> Hasql.Pool -> AppM (Either Text [DostavistaPackage])
fetchDostavistaPackages ordersId pool =
  fmap (first (pack . show)) $ 
    runTransactionM pool Hasql.Read $
      Hasql.statement (V.fromList ordersId) $
      rmap (V.toList . V.map (extractADT . convertFromJson)) $
      [Hasql.vectorStatement|
        SELECT
          jsonb_build_object(
            'ware_code', o.id,
            'description', oi.description,
            'items_count', oi.length,
            'item_payment_amount', CAST(oi.price AS text)
          ) :: jsonb
        FROM orders AS o
        INNER JOIN (
          SELECT
            moi.order_id,
            STRING_AGG(moi.item_name, ', ') AS description,
            SUM(moi.length_m) AS length,
            SUM(moi.total_price) AS price
          FROM manual_order_items AS moi
          GROUP BY moi.order_id

          UNION

          SELECT
            ofb.order_id,
            STRING_AGG(f.name, ', ') AS description,
            SUM(COALESCE(ofb.length_m, pc.length_m)) AS length,
            SUM(CASE 
             WHEN pre_cut_id IS NULL THEN
              ROUND(f.price_per_meter * (1 - f.discount) * ofb.length_m)
             ELSE ROUND(pc.price_rub * (1 - f.discount))
            END) AS price
          FROM order_fabric_bindings AS ofb
          INNER JOIN fabrics AS f
          ON ofb.fabric_id = f.id
          LEFT JOIN pre_cuts AS pc
          ON ofb.pre_cut_id = pc.id
          GROUP BY ofb.order_id

          UNION

          SELECT
            so.order_id AS order_id,
            STRING_AGG(COALESCE(f.name, fpc.name), ', ') AS description,
            SUM(COALESCE(soi.length_m, pc.length_m)) AS length,
            SUM(CASE 
             WHEN pc.id IS NULL THEN
              ROUND(f.price_per_meter * (1 - f.discount) * soi.length_m)
             ELSE ROUND(pc.price_rub * (1 - f.discount))
            END) AS price
          FROM shelf_order_items AS soi
          INNER JOIN shelf_orders AS so
          ON soi.shelf_order_id = so.id
          LEFT JOIN fabrics AS f
          ON soi.fabric_id = f.id
          LEFT JOIN pre_cuts AS pc
          ON soi.pre_cut_id = pc.id
          LEFT JOIN fabrics AS fpc
          ON pc.fabric_id = fpc.id
          GROUP BY so.order_id
        ) AS oi
        ON o.id = oi.order_id
        WHERE o.id = ANY($1 :: text[])
        ORDER BY o.id ASC
      |]


fetchSpecialPostDetails :: FabricLifecycle -> Double -> Hasql.Pool -> AppM (Either Text SpecialPostDetails)
fetchSpecialPostDetails lifeCycle threshold pool =
  fmap (first (pack . show)) $ 
    runTransactionM pool Hasql.Read $
      Hasql.statement (encodeToText lifeCycle, threshold) $
      rmap (extractADT . convertFromJson @SpecialPostDetails) $
      [Hasql.singletonStatement|
        WITH relevant_fabrics AS (
          SELECT
            f.name,
            f.thumbnail_url AS thumbnail,
            f.lifecycle_changed_at,
            ROUND(100 * f.discount) AS discount
          FROM fabrics AS f
          LEFT JOIN (
            SELECT
            fabric_id,
            COUNT(id) AS in_stock_pre_cuts_count
            FROM pre_cuts
            WHERE in_stock = TRUE
            GROUP BY fabric_id
          ) AS pc
          ON pc.fabric_id = f.id
          WHERE f.lifecycle = CAST($1 :: text AS fabric_lifecycle)
          AND (f.available_length_m >= $2 :: float8
               OR (COALESCE(pc.in_stock_pre_cuts_count, 0) > 0
                   AND f.available_length_m < $2 :: float8))
        ),
        fabric_summary AS (
          SELECT
            (SELECT COUNT(*) FROM relevant_fabrics) AS total_count,
            (SELECT 
              array_agg(
               jsonb_build_object(
                'name', name,
                'discount', discount
              ))
             FROM (
              SELECT name, discount
              FROM relevant_fabrics
              ORDER BY lifecycle_changed_at ASC
             ) AS ordered_items
            ) AS all_items,
            (SELECT 
             array_agg(random_thumbnails.thumbnail)
             FROM (
              SELECT thumbnail
              FROM relevant_fabrics
              ORDER BY random()
              LIMIT 9
             ) AS random_thumbnails
            ) AS random_thumbnail_urls
        )
        SELECT
          jsonb_build_object(
           'message_id',  sp.message_id :: int8?,
           'posted_at', sp.posted_at :: timestamptz?,
           'items_count', fs.total_count :: int4,
           'items', COALESCE(fs.all_items, '{}') :: jsonb[],
           'random_thumbnail_urls', COALESCE(fs.random_thumbnail_urls, '{}') :: text[]
          ) :: jsonb
        FROM fabric_summary AS fs
        LEFT JOIN special_posts AS sp
        ON sp.post_type = CAST($1 :: text AS special_post_type) 
        AND sp.is_active = TRUE
      |]

insertNewSpecialPost :: Int64 -> FabricLifecycle -> Hasql.Pool -> AppM (Either Text ())
insertNewSpecialPost msgId lifeCycle pool =
  fmap (first (pack . show)) $ 
    runTransactionM pool Hasql.Write $
      Hasql.statement (msgId, encodeToText lifeCycle) $
      [Hasql.resultlessStatement|
        INSERT INTO 
        special_posts (message_id,post_type) 
        VALUES ($1 :: int8, CAST($2 :: text AS special_post_type))
      |]

deleteSpecialPost :: Int64 -> Hasql.Pool -> AppM (Either Text ())
deleteSpecialPost msgId pool =
  fmap (first (pack . show)) $ 
    runTransactionM pool Hasql.Write $
      Hasql.statement (msgId) $
      [Hasql.resultlessStatement|
        UPDATE special_posts
        SET is_active = FALSE
        WHERE message_id = $1 :: int8
      |]


saveTemporaryNotificationMessage :: Int64 -> Int64 -> Hasql.Pool -> AppM (Either Text ())
saveTemporaryNotificationMessage channelId msgId pool =
  fmap (first (pack . show)) $ 
    runTransactionM pool Hasql.Write $
      Hasql.statement (channelId, msgId) $
      [Hasql.resultlessStatement|
        INSERT INTO temporary_notification_messages 
        (channel_id, message_id) 
        VALUES ($1 :: int8, $2 :: int8)
        ON CONFLICT (channel_id, message_id) DO NOTHING
      |]

sweepTemporaryNotificationMessages :: Hasql.Pool -> AppM (Either Text [Int64])
sweepTemporaryNotificationMessages pool =
  fmap (first (pack . show)) $ 
    runTransactionM pool Hasql.Write $
      Hasql.statement () $
       rmap (V.toList) $
       [Hasql.vectorStatement|
         DELETE FROM temporary_notification_messages
         WHERE created_at < NOW() - INTERVAL '1 day'
         RETURNING message_id :: int8
       |]


data InitShelf = ShelfSuccess Int64 | ShelfAlready | ShelfCapacityExceeded
 deriving Show


initShelf :: Int64 -> Int32 -> ShelfRequest -> Hasql.Pool -> AppM (Either Text InitShelf)
initShelf userId totalShelves shelfRequest pool =
  fmap (first (pack . show)) $ 
    runTransactionM pool Hasql.Write $ do

      isShelfAvailable <-
        Hasql.statement (totalShelves) $
         [Hasql.singletonStatement|
          SELECT (COUNT(*) < $1 :: int4) :: bool
          FROM shelves|]
      
      let handleInit Nothing = ShelfAlready
          handleInit (Just shelfId) = ShelfSuccess shelfId

      if isShelfAvailable then
        Hasql.statement (shelfRequest) $
         dimap (consT userId . del3 . $(recordToTuple ''ShelfRequest)) handleInit
         [Hasql.maybeStatement|
          INSERT INTO shelves
          (telegram_user_id
          , user_initials
          , user_phone) 
          VALUES ($1 :: int8, $2 :: text, $3 :: text)
          ON CONFLICT (telegram_user_id) DO NOTHING
          RETURNING id :: int8
         |]
      else Hasql.statement () $ 
             rmap (const ShelfCapacityExceeded) $ 
               [Hasql.singletonStatement|SELECT 1 :: int4|]

fetchShelfItems :: Int64 -> Hasql.Pool -> AppM (Either Text (Maybe (Maybe UTCTime, [ShelfItems])))
fetchShelfItems userId pool =
  fmap (first (pack . show)) $ 
    runTransactionM pool Hasql.Read $
      Hasql.statement (userId) $
      rmap (fmap (second (map (extractADT . convertFromJson @ShelfItems) . V.toList)))
      [Hasql.maybeStatement|
        SELECT
         s.first_item_added_at :: timestamptz?,
         COALESCE(shelf_items.items, '{}' :: jsonb[]) :: jsonb[] AS items
        FROM shelves AS s
        LEFT JOIN (
         SELECT
         shelf_id,
         array_agg(item_json) AS items
         FROM (
          SELECT
           si.shelf_id,
           jsonb_build_object(
            'article', f.article,
            'name', f.name,
            'fabric_type',
             CASE
              WHEN si.pre_cut_id IS NOT NULL 
              THEN 'pre_cut'
              ELSE 'roll'
             END,
            'quantity', 
             CASE 
              WHEN pc.id IS NULL THEN
              si.length_m
              ELSE 1.0
             END,
            'price',
             CASE
              WHEN pc.id IS NULL THEN
               ROUND(f.price_per_meter * (1 - f.discount))
              ELSE
               ROUND(pc.price_rub * (1 - f.discount))
             END
           ) AS item_json
          FROM shelf_items AS si
          INNER JOIN fabrics AS f 
          ON si.fabric_id = f.id
          LEFT JOIN pre_cuts AS pc 
          ON pc.fabric_id = f.id
          WHERE si.status = 'ON_SHELF'
          ) AS item_details
          GROUP BY shelf_id
        ) AS shelf_items 
        ON s.id = shelf_items.shelf_id
        WHERE s.telegram_user_id = $1 :: int8
      |]

getPutOnDShelfDetails ::  Int64 -> Hasql.Pool -> AppM (Either Text (Maybe PutOnShelfDetails))
getPutOnDShelfDetails userId pool = 
  fmap (first (pack . show)) $
    runTransactionM pool Hasql.Read $ 
      userId `Hasql.statement` getPutOnDShelfDetailsStatement

getPutOnDShelfDetailsStatement :: Hasql.Statement Int64 (Maybe PutOnShelfDetails)
getPutOnDShelfDetailsStatement =
  rmap (fmap (extractADT . convertFromJson @PutOnShelfDetails))
  [Hasql.maybeStatement|
    WITH cart_items AS (
      SELECT
        c.telegram_user_id,
        jsonb_build_object(
          'name', f.name,
          'article', f.article,
          'total_price', ROUND(f.price_per_meter * (1 - f.discount) * ci.length_m),
          'fabric_type', ci.item_type,
          'price_per_metre', ROUND(f.price_per_meter * (1 - f.discount)),
          'length_m', ci.length_m,
          'telegram_url', ci.telegram_url,
          'thumbnail_url', f.thumbnail_url
        ) :: jsonb AS item
      FROM carts AS c
      INNER JOIN cart_items AS ci
      ON c.id = ci.cart_id
      INNER JOIN fabrics AS f
      ON f.id = ci.fabric_id
      WHERE c.telegram_user_id = $1 :: int8

      UNION ALL

      SELECT
        c.telegram_user_id,
        jsonb_build_object(
          'name', f.name,
          'article', f.article,
          'total_price', ROUND(pc.price_rub * (1 - f.discount)),
          'fabric_type', ci.item_type,
          'price_per_metre', null,
          'length_m', null,
          'telegram_url', ci.telegram_url,
          'thumbnail_url', f.thumbnail_url
        ) :: jsonb AS item
      FROM carts AS c
      INNER JOIN cart_items AS ci
      ON c.id = ci.cart_id
      INNER JOIN pre_cuts AS pc
      ON pc.id = ci.pre_cut_id
      INNER JOIN fabrics AS f
      ON f.id = pc.fabric_id
      WHERE c.telegram_user_id = $1 :: int8)
    SELECT
     jsonb_build_object(
      'shelf_id', s.id,
      'user_initials', s.user_initials,
      'phone', s.user_phone,
      'items', ci.items,
      'items_on_shelf_count', 
       COALESCE(COUNT(si.id) FILTER (WHERE si.status = 'ON_SHELF'), 0)
     ) :: jsonb
    FROM shelves AS s
    LEFT JOIN shelf_items AS si
    ON s.id = si.shelf_id
    INNER JOIN (
      SELECT
        telegram_user_id,
        array_agg(item) AS items
      FROM cart_items
      GROUP BY telegram_user_id
    ) AS ci
    ON s.telegram_user_id = ci.telegram_user_id
    GROUP BY s.id, s.user_initials, s.user_phone, ci.items
  |]


finalizeShelfCheckout :: Int64 -> Text -> Int64 -> NewPaymentRecord -> Hasql.Pool -> AppM (Either Text ())
finalizeShelfCheckout userId orderId notificationId paymentRecord pool =
  fmap (first (pack . show)) $
    runTransactionM pool Hasql.Write $ do
      -- Step 1: Create the main shelf_order record.
     Hasql.statement (userId, orderId, notificationId) $
       createShelfOrderStatement userId orderId notificationId
     -- Step 2: Create the associated payment record.
     Hasql.statement paymentRecord $
       insertNewPaymentRecordStatement
     -- Step 3: Clear the user's cart.
     Hasql.statement userId $ clearCartStatement

type OrderItemRaw = (Text, Text, Text, Maybe Double, Double, Maybe Double)

createShelfOrderStatement :: Int64 -> Text -> Int64 -> Hasql.Statement (Int64, Text, Int64) ()
createShelfOrderStatement userId orderId notifcationId =
    [Hasql.resultlessStatement|
      WITH new_order AS (
        INSERT INTO shelf_orders
        (order_id, shelf_id, status, internal_notification_message_id)
        SELECT 
        $2 :: text, 
        id :: int8, 
        'registered' :: shelf_order_status,
        $3 :: int8
        FROM shelves WHERE telegram_user_id = $1 :: int8
        RETURNING id :: int8
      )
      INSERT INTO shelf_order_items
      (shelf_order_id, fabric_id, pre_cut_id, length_m)
      SELECT
      (SELECT id FROM new_order) :: int8,
      COALESCE(ci.fabric_id, pc.fabric_id),
      ci.pre_cut_id,
      ci.length_m
      FROM carts AS c
      INNER JOIN cart_items AS ci
      ON c.id = ci.cart_id
      LEFT JOIN pre_cuts AS pc
      ON pc.id = ci.pre_cut_id
      WHERE c.telegram_user_id = $1 :: int8 
    |]

updateShelfOrderStatusStatement :: Hasql.Statement (Text, ShelfOderStatus) (Maybe Int64)
updateShelfOrderStatusStatement =
  lmap (second encodeToText) $
  [Hasql.maybeStatement|
    UPDATE shelf_orders
    SET status = CAST($2 :: text AS shelf_order_status)
    WHERE order_id = $1 :: text
    RETURNING COALESCE(internal_notification_message_id, 0) :: int8
  |]


setFirstItemAddedStatement :: Hasql.Statement Text ()
setFirstItemAddedStatement =
  [Hasql.resultlessStatement|
    WITH shelf_ident AS (
     SELECT shelf_id 
     FROM shelf_orders
     WHERE order_id = $1 :: text
    )
    UPDATE shelves
    SET first_item_added_at =
     CASE WHEN
      (SELECT COUNT(*) 
       FROM shelf_items 
       WHERE shelf_id = 
       (SELECT shelf_id FROM shelf_ident)) = 0 
      THEN
      NOW()
      ELSE first_item_added_at
     END
    WHERE id = (SELECT shelf_id FROM shelf_ident)
  |]  


moveItemsToShelfStatement :: Hasql.Statement Text ()
moveItemsToShelfStatement =
  [Hasql.resultlessStatement|
    WITH shelf_info AS (
      SELECT
        so.shelf_id,
        so.id AS shelf_order_id
      FROM shelf_orders AS so
      WHERE so.order_id = $1 :: text
    ),
    items_to_move AS (
      SELECT
        soi.fabric_id,
        soi.pre_cut_id,
        soi.length_m
      FROM shelf_order_items AS soi
      INNER JOIN shelf_info AS si
      ON soi.shelf_order_id = si.shelf_order_id
    )
    INSERT INTO shelf_items
    (shelf_id, fabric_id, pre_cut_id, length_m)
    SELECT
      si.shelf_id,
      itm.fabric_id,
      itm.pre_cut_id,
      itm.length_m
    FROM items_to_move AS itm
    INNER JOIN shelf_info AS si ON TRUE
  |]

fetchShelfItemsForShipment :: Int64 -> Hasql.Pool -> AppM (Either Text (Maybe ShelfItemsForShipment))
fetchShelfItemsForShipment userId pool =
  fmap (first (pack . show)) $
    runTransactionM pool Hasql.Read $ 
      userId `Hasql.statement` fetchShelfItemsForShipmentStatement

fetchShelfItemsForShipmentStatement :: Hasql.Statement Int64 (Maybe ShelfItemsForShipment)
fetchShelfItemsForShipmentStatement =
  rmap (fmap (extractADT . convertFromJson @ShelfItemsForShipment))
  [Hasql.maybeStatement|
    SELECT
     jsonb_build_object(
      'shelf_id', s.id,
      'user_initials', s.user_initials,
      'phone', s.user_phone,
      'items', ci.items) :: jsonb
    FROM shelves AS s
    INNER JOIN (
      SELECT
       shelf_id,
       array_agg(
        jsonb_build_object(
         'id', si.id,
         'name', f.name,
         'article', f.article,
         'total_price', 
          CASE 
           WHEN pc.id IS NULL THEN
            ROUND(f.price_per_meter * (1 - f.discount) * si.length_m)
           ELSE
            ROUND(pc.price_rub * (1 - f.discount))
          END,
         'fabric_type',
          CASE 
           WHEN pc.id IS NULL THEN
            'roll'
           ELSE
            'pre_cut'
           END,
          'price_per_metre',
           CASE 
            WHEN pc.id IS NULL THEN
             ROUND(f.price_per_meter * (1 - f.discount))
            ELSE
             NULL
           END,
          'length_m', si.length_m,
          'telegram_url', '',
          'thumbnail_url', 
           CASE 
            WHEN pc.id IS NULL THEN
             f.thumbnail_url
            ELSE
             fpc.thumbnail_url
           END
          )) AS items
      FROM shelf_items AS si
      INNER JOIN fabrics AS f
      ON si.fabric_id = f.id
      LEFT JOIN pre_cuts AS pc
      ON pc.fabric_id = f.id
      LEFT JOIN fabrics AS fpc
      ON pc.fabric_id = fpc.id
      WHERE si.status = 'ON_SHELF'
      GROUP BY shelf_id
    ) AS ci
    ON s.id = ci.shelf_id
    WHERE s.telegram_user_id = $1 :: int8
  |]


placeNewShelfOrder :: Order -> Hasql.Pool -> AppM (Either Text ())
placeNewShelfOrder order pool =
  fmap (first (pack . show)) $
    runTransactionM pool Hasql.Write $ do
      shelfItemIds <- Hasql.statement order placeNewShelfOrderStatement
      Hasql.statement (_orderId order, shelfItemIds) setShelfItemShippedStatememt
      Hasql.statement (_orderTelegramUserId order) resetFirstItemAddedAtStatememt


placeNewShelfOrderStatement :: Hasql.Statement Order (V.Vector Int64)
placeNewShelfOrderStatement =
 lmap $(recordToTuple ''Order)
  [Hasql.singletonStatement|
    INSERT INTO orders (
     id,
     customer_full_name,
     customer_phone,
     delivery_provider_id,
     delivery_point_id,
     sdek_request_uuid,
     sdek_tracking_number,
     internal_notification_message_id,
     tariff,
     created_at,
     updated_at,
     status,
     is_bot,
     actual_weight_grams
    ) VALUES (
       $1 :: text,
       $2 :: text,
       $3 :: text,
       $4 :: text,
       $5 :: text,
       $6 :: uuid,
       $7 :: text,
       $8 :: int8,
       $10 :: int4,
       now(),
       now(),
       'paid',
       true,
       (SELECT
         SUM(ROUND(
         COALESCE(si.length_m, pc.length_m) *
         COALESCE(f.weight_per_metre, pcf.weight_per_metre)))
       FROM shelves AS s
       INNER JOIN shelf_items AS si
       ON si.shelf_id = s.id
       LEFT JOIN fabrics AS f
       ON f.id = si.fabric_id
       LEFT JOIN pre_cuts AS pc
       ON pc.id = si.pre_cut_id
       LEFT JOIN fabrics AS pcf
       ON pcf.id = pc.fabric_id
       WHERE s.telegram_user_id = $9 :: int8
       AND si.status = 'ON_SHELF'
       ))
       RETURNING (
        SELECT array_agg(si.id)
        FROM shelves AS s
        INNER JOIN shelf_items AS si
        ON si.shelf_id = s.id
        WHERE s.telegram_user_id = $9 :: int8
        AND si.status = 'ON_SHELF'
       ) :: int8[]
  |]

setShelfItemShippedStatememt :: Hasql.Statement (Text, V.Vector Int64) ()
setShelfItemShippedStatememt =
  [Hasql.resultlessStatement|
    UPDATE shelf_items
    SET status = 'SHIPPED',
        main_order_id = $1 :: text
    WHERE id = ANY($2 :: int8[])
  |]

resetFirstItemAddedAtStatememt :: Hasql.Statement Int64 ()
resetFirstItemAddedAtStatememt =
  [Hasql.resultlessStatement|
    UPDATE shelves
    SET first_item_added_at = NULL
    WHERE telegram_user_id = $1 :: int8
  |]

getShelfStatus :: Int64 -> Hasql.Pool -> AppM (Either Text ShelfStatus)
getShelfStatus userId pool =
  fmap (first (pack . show)) $
    runTransactionM pool Hasql.Read $
      Hasql.statement (userId) $
       rmap (fromMaybe Absent . (fmap (extractADT . convertFromJson @ShelfStatus)))
       [Hasql.maybeStatement|
        SELECT
        to_jsonb(status) :: jsonb
        FROM shelves 
        WHERE telegram_user_id = $1 :: int8
       |]

saveShelfSubmissionInfo :: ShelfSubmissionChatDetails -> Hasql.Pool -> AppM (Either Text ())
saveShelfSubmissionInfo submission pool =
  fmap (first (pack . show)) $
    runTransactionM pool Hasql.Write $
      Hasql.statement submission $
       lmap $(recordToTuple ''ShelfSubmissionChatDetails)
       [Hasql.resultlessStatement|
        INSERT INTO shelf_submissions
        (telegram_user_id, chat_id, message_id)
        VALUES ($1 :: int8, $2 :: int8, $3 :: int8)
        ON CONFLICT (chat_id, message_id) DO NOTHING
       |]

getShelfPersonalInfo :: Int64 -> Hasql.Pool -> AppM (Either Text (Maybe Text, Maybe Text, Maybe Text))
getShelfPersonalInfo userId pool =
  fmap (first (pack . show)) $
    runTransactionM pool Hasql.Read $
     Hasql.statement (userId) $
     [Hasql.singletonStatement|
      SELECT
      user_initials :: text?,
      user_phone :: text?,
      preferred_sdek_point :: text?
      FROM shelves
      WHERE telegram_user_id = $1 :: int8
     |]

editShelfPersonalInfo :: Int64 -> ShelfPersonalInfo -> Hasql.Pool -> AppM (Either Text ())
editShelfPersonalInfo userId personalInfo pool =
  fmap (first (pack . show)) $
    runTransactionM pool Hasql.Write $
    Hasql.statement (personalInfo) $
     lmap (consT userId . $(recordToTuple ''ShelfPersonalInfo)) $
     [Hasql.resultlessStatement|
      UPDATE shelves
      SET user_initials = COALESCE($2 :: text?, user_initials),
          user_phone = COALESCE($3 :: text?, user_phone),
          preferred_sdek_point = COALESCE($4 :: text?, preferred_sdek_point)
      WHERE telegram_user_id = $1 :: int8
     |]


getAppStatusDetails :: [SdekPickupAppStatus] -> Hasql.Pool -> AppM (Either Text (Maybe (Int64, UUID, SdekPickupAppStatus)))
getAppStatusDetails statuses pool =
  fmap (first (pack . show)) $
    runTransactionM pool Hasql.Read $
    Hasql.statement (map encodeToText statuses) $
     dimap V.fromList (fmap (app3 (extractADT . convertFromJson @SdekPickupAppStatus))) $
     [Hasql.maybeStatement|
       SELECT
        id :: int8,
        app_uuid :: uuid,
        to_jsonb(app_status) :: jsonb
       FROM courier_pickups
       WHERE app_status = ANY($1 :: text[])
     |]

updatePickupAppStatus :: Int64 -> SdekPickupAppStatus -> Hasql.Pool -> AppM (Either Text ())
updatePickupAppStatus id status pool =
 fmap (first (pack . show)) $
    runTransactionM pool Hasql.Write $
    Hasql.statement (id, encodeToText status) $
     [Hasql.resultlessStatement|
       UPDATE courier_pickups
       SET app_status = $2 :: text
       WHERE id = $1 :: int8
     |]

updatePickedUpOrdersStatus :: Int64 -> OrderStatus -> Hasql.Pool -> AppM (Either Text ())
updatePickedUpOrdersStatus pickupId status pool = 
  fmap (first (pack . show)) $
    runTransactionM pool Hasql.Write $
    Hasql.statement (pickupId, encodeToText status) $
     [Hasql.resultlessStatement|
       UPDATE orders
       SET status = CAST($2 :: text AS order_status)
       WHERE sdek_courier_pickup_id = $1 :: int8
     |]

addMediaToFabric :: FabricMediaRequest -> Hasql.Pool -> AppM (Either Text ())
addMediaToFabric media pool =
  fmap (first (pack . show)) $
    runTransactionM pool Hasql.Write $
    Hasql.statement media $
     lmap ( app2 encodeToText 
          . app4 encodeToText 
          . $(recordToTuple ''FabricMediaRequest))
     [Hasql.resultlessStatement|
       INSERT INTO fabric_media
       ( fabric_parent_id
       , fabric_type
       , telegram_file_id
       , media_type)
       VALUES ($1 :: int8, $2 :: text, $3 :: text, $4 :: text)
     |]


fetchCancelledOrders :: Hasql.Pool -> AppM (Either Text [CancelledOrders])
fetchCancelledOrders pool = 
  fmap (first (pack . show)) $
    runTransactionM pool Hasql.Read $
      Hasql.statement () $
       rmap (map (extractADT . convertFromJson @CancelledOrders) . V.toList) $
       [Hasql.vectorStatement|
         SELECT
           jsonb_build_object(
            'order_id', id,
            'channel', to_jsonb('main' :: text), 
            'message_id', COALESCE(internal_notification_message_id, 0))
            :: jsonb
         FROM orders
         WHERE status = 'cancelled'
         AND is_erased = FALSE
          
         UNION ALL

         SELECT 
           jsonb_build_object(
            'order_id', order_id,
            'channel', to_jsonb('shelf' :: text), 
            'message_id', COALESCE(internal_notification_message_id, 0))
            :: jsonb
         FROM shelf_orders
         WHERE status = 'cancelled'
         AND is_erased = FALSE   
       |]

markedCancelledOrders :: [(Text, ChatKey)] -> Hasql.Pool -> AppM (Either Text ())
markedCancelledOrders ids pool =
  fmap (first (pack . show)) $
    runTransactionM pool Hasql.Write $ do
     
     let mainOrders = V.fromList $ fst $ unzip $ filter (\(_, channel) -> channel == MAIN) ids
     let shelfOrders = V.fromList $ fst $ unzip $ filter (\(_, channel) -> channel == SHELF) ids

     Hasql.statement (mainOrders) $
      [Hasql.resultlessStatement|
       UPDATE orders
       SET is_erased = TRUE
       WHERE id = ANY($1 :: text[])
       AND status = 'cancelled' 
       AND is_erased = FALSE
      |]

     Hasql.statement (shelfOrders) $
      [Hasql.resultlessStatement|
       UPDATE shelf_orders
       SET is_erased = TRUE
       WHERE order_id = ANY($1 :: text[])
       AND status = 'cancelled' 
       AND is_erased = FALSE
      |]

fetchOrderDetailsForYaml :: Text -> Hasql.Pool -> AppM (Either Text OrderDetailsForYaml)
fetchOrderDetailsForYaml orderId pool =
  fmap (first (pack . show)) $
    runTransactionM pool Hasql.Read $
      Hasql.statement (orderId) $
     rmap (extractADT . convertFromJson @OrderDetailsForYaml)
     [Hasql.singletonStatement|
       SELECT
        jsonb_build_object(
         'customer_full_name', 
         o.customer_full_name,
         'customer_phone', 
         o.customer_phone,
         'delivery_provider_id', 
         to_jsonb(o.delivery_provider_id),
         'delivery_point_id', 
         o.delivery_point_id,
         'physical_dimensions',
         jsonb_build_object(
          'length', o.length,
          'width', o.width,
          'height', o.height),
         'items',
          array_agg(
           jsonb_build_object(
            'name', 
             CASE 
               WHEN pc.id IS NULL 
               THEN f.name 
               ELSE fpc.name 
             END,
            'fabric_type', 
             CASE 
              WHEN pc.id IS NULL 
              THEN 'roll' 
              ELSE 'pre_cut' 
             END,
            'total_price', 
             CASE 
              WHEN pc.id IS NULL THEN 
              ROUND(f.price_per_meter * (1 - f.discount) * ofb.length_m)
              ELSE ROUND(pc.price_rub * (1 - f.discount)) 
             END,
            'length_m', COALESCE(ofb.length_m, pc.length_m),
            'weight', 
              COALESCE(ofb.length_m, pc.length_m) * 
                       COALESCE(f.weight_per_metre, fpc.weight_per_metre)
           ))) ::jsonb
        FROM orders AS o
        INNER JOIN order_fabric_bindings AS ofb
        ON o.id = ofb.order_id
        LEFT JOIN fabrics AS f
        ON ofb.fabric_id = f.id
        LEFT JOIN pre_cuts AS pc
        ON ofb.pre_cut_id = pc.id
        LEFT JOIN fabrics AS fpc
        ON pc.fabric_id = fpc.id
        WHERE o.id = $1 :: text
        GROUP BY o.id, o.customer_full_name, o.customer_phone, o.delivery_provider_id, o.delivery_point_id, o.length, o.width, o.height

        
        UNION ALL

        SELECT
        jsonb_build_object(
         'customer_full_name', 
         o.customer_full_name,
         'customer_phone', 
         o.customer_phone,
         'delivery_provider_id', 
         to_jsonb(o.delivery_provider_id),
         'delivery_point_id', 
         o.delivery_point_id,
         'physical_dimensions',
         jsonb_build_object(
          'length', o.length,
          'width', o.width,
          'height', o.height),
         'items',
          array_agg(
           jsonb_build_object(
            'name', 
             CASE 
               WHEN pc.id IS NULL 
               THEN f.name 
               ELSE fpc.name 
             END,
            'fabric_type', 
             CASE 
              WHEN pc.id IS NULL 
              THEN 'roll' 
              ELSE 'pre_cut' 
             END,
            'total_price', 
             CASE 
              WHEN pc.id IS NULL THEN 
              ROUND(f.price_per_meter * (1 - f.discount) * si.length_m)
              ELSE ROUND(pc.price_rub * (1 - f.discount)) 
             END,
            'length_m', COALESCE(si.length_m, pc.length_m),
            'weight', 
              COALESCE(si.length_m, pc.length_m) * 
                       COALESCE(f.weight_per_metre, fpc.weight_per_metre)
           ))) ::jsonb
        FROM orders AS o
        INNER JOIN shelf_items AS si
        ON o.id = si.main_order_id
        LEFT JOIN fabrics AS f
        ON si.fabric_id = f.id
        LEFT JOIN pre_cuts AS pc
        ON si.pre_cut_id = pc.id
        LEFT JOIN fabrics AS fpc
        ON pc.fabric_id = fpc.id
        WHERE o.id = $1 :: text
        GROUP BY o.id, o.customer_full_name, o.customer_phone, o.delivery_provider_id, o.delivery_point_id, o.length, o.width, o.height
     |]

fetchLostParcels :: Hasql.Pool -> AppM (Either Text [Text])
fetchLostParcels pool =
  fmap (first (pack . show)) $
    runTransactionM pool Hasql.Read $
      Hasql.statement () $
      rmap V.toList
      [Hasql.vectorStatement|
       SELECT id :: text
       FROM orders 
       WHERE status = 'picked_up_by_courier' 
       AND NOW() - updated_at > interval '1 day'
      |]

fetchPreferredSdekPoint :: Int64 -> Hasql.Pool -> AppM (Either Text (Maybe Text))
fetchPreferredSdekPoint userId pool =
  fmap (first (pack . show)) $
    runTransactionM pool Hasql.Read $
     Hasql.statement (userId) $
     [Hasql.singletonStatement|
      SELECT preferred_sdek_point :: text?
      FROM shelves
      WHERE telegram_user_id = $1 :: int8
     |]
    
removePreferredSdekPoint :: Int64 -> Hasql.Pool -> AppM (Either Text ())
removePreferredSdekPoint userId pool =
  fmap (first (pack . show)) $
    runTransactionM pool Hasql.Write $
     Hasql.statement (userId) $
     [Hasql.singletonStatement|
      UPDATE shelves
      SET preferred_sdek_point = NULL
      WHERE telegram_user_id = $1 :: int8
     |]