-- We need these extensions for servant-generic
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeOperators     #-}

module API where

import GHC.Generics (Generic)
import Servant.API.Generic
import Data.Text (Text)
import Servant (Get, Post, Put, Patch, Delete, PlainText, Capture, JSON, (:>), ReqBody, QueryParam)
import Data.Proxy (Proxy (..))
import Data.Int (Int64)
import Data.Time.TypeLevel (TimePeriod(Minute, Second))

import API.Types
import Lib.Servant.RateLimit (RateLimitPerUser)
import API.WithField (WithField)
import Domain.Warehouse.Types (FabricType)
import Domain.Warehouse.Enums (FabricLifecycle)


-- This 'data' definition IS our new API ADT.
-- The 'route' parameter is a placeholder that Servant uses.
data Routes route = Routes
  {
    _putNewFabric 
       :: route
       :- "warehouse"
       :> "fabric"
       :> RateLimitPerUser (Second 1) 'Nothing
       :> ReqBody '[JSON] RawIngestRequest
       :> Put '[JSON] (ApiResponse NewFabric)
  , _patchFabric
       :: route
       :- "warehouse"
       :> "fabric"
       :> RateLimitPerUser (Second 1) 'Nothing
       :> Capture "fabric_id" Int64
       :> ReqBody '[JSON] RawIngestRequest
       :> Patch '[JSON] (ApiResponse NewFabric)
  , _deleteFabric
       :: route
       :- "warehouse"
       :> "fabric"
       :> RateLimitPerUser (Second 1) 'Nothing
       :> Capture "fabric_id" Int64
       :> Capture "fabric_type" FabricType
       :> Delete '[JSON] (ApiResponse ())  
  , _getFabricPreview 
       :: route 
       :- "fabric"
       :> "preview"
       :> RateLimitPerUser (Second 1) 'Nothing
       :> QueryParam "fabric_id" Int64
       :> QueryParam "fabric_type" FabricType
       :> Get '[JSON] (ApiResponse FabricPreview)
  , _getDeliveryPoints
       :: route 
       :- "providers"
       :> RateLimitPerUser (Second 1) 'Nothing
       :> Capture "provider" Providers 
       :> "delivery-points" 
       :> QueryParam "city" Text 
       :> Get '[JSON] (ApiResponse [WithField "dpMetros" [Text] DeliveryPoint])
  , _getProviders
       :: route
       :- "providers"
       :> RateLimitPerUser (Second 1) 'Nothing
       :> Get '[JSON] (ApiResponse [ProviderInfo])
  , _placeNewOrder
       :: route
       :- "order"
       :> "create"
       :> RateLimitPerUser (Second 1) 'Nothing
       :> ReqBody '[JSON] OrderRequest
       :> Post '[JSON] (ApiResponse OrderConfirmationDetails)
  , _setTelegramMessage
       :: route
       :- "order"
       :> "set_telegram_message"
       :> RateLimitPerUser (Second 1) 'Nothing
       :> ReqBody '[JSON] SetTelegramMessageRequest
       :> Post '[JSON] (ApiResponse ())
  , _trackOrder
       :: route
       :- "order"
       :> "track"
       :> RateLimitPerUser (Second 1) 'Nothing
       :> QueryParam "query" Text
       :> Get '[JSON] (ApiResponse (Maybe TrackOrder))
     -- deprecated, will be replaced by fixed catalogs: regular, on sale, clearance
  , _getCatalogSummary
       :: route
       :- "catalog"
       :> "by-date"
       :> RateLimitPerUser (Second 1) 'Nothing
       :> QueryParam "date" CatalogDate
       :> Get '[JSON] (ApiResponse CatalogSummary)
   , _searchFabrics 
       :: route
       :- "search"
       :> RateLimitPerUser (Second 1) 'Nothing
       :> QueryParam "query" Text
       :> QueryParam "page" Int
       :> QueryParam "limit" Int
       :> Get '[JSON] (ApiResponse (PaginatedResults SearchTeaser))
   , _searchFabricCard 
       :: route
       :- "search"
       :> RateLimitPerUser (Second 1) 'Nothing
       :> Capture "type" FabricType
       :> Capture "id" Int64
       :> Get '[JSON] (ApiResponse (Maybe CatalogSummaryItem))
   , _putDailyDigest
       :: route
       :- "warehouse"
       :> "daily-digest"
       :> RateLimitPerUser (Second 1) 'Nothing
       :> Post '[JSON] ()
  , _draftDailyDigestDraft
       :: route
       :- "warehouse"
       :> "daily-digest"
       :> "draft"
       :> RateLimitPerUser (Second 1) 'Nothing
       :> ReqBody '[JSON] DailyDigestDraft
       :> Post '[JSON] (ApiResponse ())
  , _publishDailyDigest
       :: route
       :- "warehouse"
       :> "daily-digest"
       :> "publish"
       :> RateLimitPerUser (Second 1) 'Nothing
       :> ReqBody '[JSON] DailyDigest
       :> Post '[JSON] (ApiResponse ())
  , _cancelOrder
       :: route
       :- "order"
       :> "cancel"
       :> RateLimitPerUser (Second 1) 'Nothing
       :> ReqBody '[JSON] CancelOrder
       :> Post '[JSON] (ApiResponse ())
  , _checkCartItem
       :: route
       :- "cart"
       :> "check-item"
       :> RateLimitPerUser (Second 1) 'Nothing
       :> QueryParam "user_id" Int64
       :> QueryParam "fabric_id" Int64
       :> QueryParam "fabric_type" FabricType
       :> Get '[JSON] (ApiResponse CheckItemInCart)
  , _addToCart 
       :: route
       :- "cart"
       :> "add"
       :> RateLimitPerUser (Second 1) 'Nothing
       :> ReqBody '[JSON] CartNewFabric
       :> Post '[JSON] (ApiResponse CartCheckStatus)
  , _clearCart
       :: route
       :- "cart"
       :> "clear"
       :> RateLimitPerUser (Second 1) 'Nothing
       :> QueryParam "user_id" Int64
       :> Get '[JSON] (ApiResponse ())
  , _viewCart
       :: route
       :- "cart"
       :> RateLimitPerUser (Second 1) 'Nothing
       :> QueryParam "user_id" Int64
       :> Get '[JSON] (ApiResponse ViewCart)
  , _measureOrder
       :: route
       :- "warehouse"
       :> "orders"
       :> "measure"
       :> RateLimitPerUser (Second 1) 'Nothing
       :> ReqBody '[JSON] MeasureRequest
       :> Post '[JSON] (ApiResponse MeasureResponse)
  , _placeYamlOrder
       :: route
       :- "warehouse"
       :> "orders"
       :> "create-from-yaml"
       :> RateLimitPerUser (Second 1) 'Nothing
       :> ReqBody '[JSON] YamlOrderRequest
       :> Put '[JSON] (ApiResponse YamlOrderResponse)
  , _reportDailySales
      :: route
      :- "warehouse"
      :> "reports"
      :> "daily-sales"
      :> RateLimitPerUser (Second 1) 'Nothing
      :> Post '[JSON] (ApiResponse ())
  , _reportMonthlySales
      :: route
      :- "warehouse"
      :> "reports"
      :> "monthly-sales"
      :> RateLimitPerUser (Second 1) 'Nothing
      :> Post '[JSON] (ApiResponse ())
  , _tallyUpExpenses
      :: route
      :- "warehouse"
      :> "expenses"
      :> "tally-up"
      :> RateLimitPerUser (Second 1) 'Nothing
      :> ReqBody '[JSON] Expenses
      :> Put '[JSON] (ApiResponse ())
  , _setOrderDimensions
      :: route
      :- "warehouse"
      :> "orders"
      :> "dimensions"
      :> RateLimitPerUser (Second 1) 'Nothing
      :> Capture "order_id" Text
      :> ReqBody '[JSON] SetOrderDimensionsRequest   
      :> Put '[JSON] (ApiResponse ())
   , _getSdekDeliveryPointUUID
       :: route
       :- "sdek"
       :> "delivery-point"
       :> RateLimitPerUser (Minute 10) ('Just "user")
       :> QueryParam "address" Text
       :> QueryParam "user" Int64
       :> Get '[JSON] (ApiResponse SdekDeliveryPoint)
   , _getCatalogSummaryV2
       :: route
       :- "catalog"
       :> RateLimitPerUser (Second 1) 'Nothing
       :> QueryParam "life-cycle" FabricLifecycle 
       :> Get '[JSON] (ApiResponse CatalogSummaryV2)

  } deriving (Generic)


tkaniApiProxy :: Proxy (ToServantApi Routes)
tkaniApiProxy = genericApi (Proxy :: Proxy Routes)