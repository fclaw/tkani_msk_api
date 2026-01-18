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
import Lib.Servant.RateLimit (RateLimitPerIP)
import API.WithField (WithField)
import Domain.Warehouse.Types (FabricType)


-- This 'data' definition IS our new API ADT.
-- The 'route' parameter is a placeholder that Servant uses.
data Routes route = Routes
  {
    _putNewFabric 
       :: route
       :- "warehouse"
       :> "fabric"
       :> RateLimitPerIP (Second 1)
       :> ReqBody '[JSON] RawIngestRequest
       :> Put '[JSON] (ApiResponse NewFabric)
  , _patchFabric
       :: route
       :- "warehouse"
       :> "fabric"
       :> RateLimitPerIP (Second 1)
       :> Capture "fabric_id" Int64
       :> ReqBody '[JSON] RawIngestRequest
       :> Patch '[JSON] (ApiResponse NewFabric)
  , _deleteFabric
       :: route
       :- "warehouse"
       :> "fabric"
       :> RateLimitPerIP (Second 1)
       :> Capture "fabric_id" Int64
       :> Capture "fabric_type" FabricType
       :> Delete '[JSON] (ApiResponse ())  
  , _getFabricPreview 
       :: route 
       :- "fabric"
       :> "preview"
       :> RateLimitPerIP (Second 1)
       :> QueryParam "fabric_id" Int64
       :> QueryParam "fabric_type" FabricType
       :> Get '[JSON] (ApiResponse FabricPreview)
  , _getDeliveryPoints
       :: route 
       :- "providers"
       :> RateLimitPerIP (Second 1)
       :> Capture "provider" Providers 
       :> "delivery-points" 
       :> QueryParam "city" Text 
       :> Get '[JSON] (ApiResponse [WithField "dpMetros" [Text] DeliveryPoint])
  , _getProviders
       :: route
       :- "providers"
       :> RateLimitPerIP (Second 1)
       :> Get '[JSON] (ApiResponse [ProviderInfo])
  , _placeNewOrder
       :: route
       :- "order"
       :> "create"
       :> RateLimitPerIP (Second 1)
       :> ReqBody '[JSON] OrderRequest
       :> Post '[JSON] (ApiResponse OrderConfirmationDetails)
  , _setTelegramMessage
       :: route
       :- "order"
       :> "set_telegram_message"
       :> RateLimitPerIP (Second 1)
       :> ReqBody '[JSON] SetTelegramMessageRequest
       :> Post '[JSON] (ApiResponse ())
  , _trackOrder
       :: route
       :- "order"
       :> "track"
       :> RateLimitPerIP (Second 1)
       :> QueryParam "query" Text
       :> Get '[JSON] (ApiResponse (Maybe TrackOrder))
  , _getCatalogSummary
       :: route
       :- "catalog"
       :> "by-date"
       :> RateLimitPerIP (Second 1)
       :> QueryParam "date" CatalogDate
       :> Get '[JSON] (ApiResponse CatalogSummary)
   , _searchFabrics 
       :: route
       :- "search"
       :> RateLimitPerIP (Second 1)
       :> QueryParam "query" Text
       :> QueryParam "page" Int
       :> QueryParam "limit" Int
       :> Get '[JSON] (ApiResponse (PaginatedResults SearchTeaser))
   , _searchFabricCard 
       :: route
       :- "search"
       :> RateLimitPerIP (Second 1)
       :> Capture "type" FabricType
       :> Capture "id" Int64
       :> Get '[JSON] (ApiResponse (Maybe CatalogSummaryItem))
   , _putDailyDigest
       :: route
       :- "warehouse"
       :> "daily-digest"
       :> RateLimitPerIP (Second 1)
       :> Post '[JSON] ()
  , _draftDailyDigestDraft
       :: route
       :- "warehouse"
       :> "daily-digest"
       :> "draft"
       :> RateLimitPerIP (Second 1)
       :> ReqBody '[JSON] DailyDigestDraft
       :> Post '[JSON] (ApiResponse ())
  , _publishDailyDigest
       :: route
       :- "warehouse"
       :> "daily-digest"
       :> "publish"
       :> RateLimitPerIP (Second 1)
       :> ReqBody '[JSON] DailyDigest
       :> Post '[JSON] (ApiResponse ())
  , _cancelOrder
       :: route
       :- "order"
       :> "cancel"
       :> RateLimitPerIP (Second 1)
       :> ReqBody '[JSON] CancelOrder
       :> Post '[JSON] (ApiResponse ())
  , _checkCartItem
       :: route
       :- "cart"
       :> "check-item"
       :> RateLimitPerIP (Second 1)
       :> QueryParam "user_id" Int64
       :> QueryParam "fabric_id" Int64
       :> QueryParam "fabric_type" FabricType
       :> Get '[JSON] (ApiResponse CheckItemInCart)
  , _addToCart 
       :: route
       :- "cart"
       :> "add"
       :> RateLimitPerIP (Second 1)
       :> ReqBody '[JSON] CartNewFabric
       :> Post '[JSON] (ApiResponse CartCheckStatus)
  , _clearCart
       :: route
       :- "cart"
       :> "clear"
       :> RateLimitPerIP (Second 1)
       :> QueryParam "user_id" Int64
       :> Get '[JSON] (ApiResponse ())
  , _viewCart
       :: route
       :- "cart"
       :> RateLimitPerIP (Second 1)
       :> QueryParam "user_id" Int64
       :> Get '[JSON] (ApiResponse ViewCart)
  , _measureOrder
       :: route
       :- "warehouse"
       :> "orders"
       :> "measure"
       :> RateLimitPerIP (Second 1)
       :> ReqBody '[JSON] MeasureRequest
       :> Post '[JSON] (ApiResponse MeasureResponse)
  , _placeYamlOrder
       :: route
       :- "warehouse"
       :> "orders"
       :> "create-from-yaml"
       :> RateLimitPerIP (Second 1)
       :> ReqBody '[JSON] YamlOrderRequest
       :> Put '[JSON] (ApiResponse YamlOrderResponse)
  , _reportDailySales
      :: route
      :- "warehouse"
      :> "reports"
      :> "daily-sales"
      :> RateLimitPerIP (Second 1)
      :> Post '[JSON] (ApiResponse ())
  , _reportMonthlySales
      :: route
      :- "warehouse"
      :> "reports"
      :> "monthly-sales"
      :> RateLimitPerIP (Second 1)
      :> Post '[JSON] (ApiResponse ())
  , _tallyUpExpenses
      :: route
      :- "warehouse"
      :> "expenses"
      :> "tally-up"
      :> RateLimitPerIP (Second 1)
      :> ReqBody '[JSON] Expenses
      :> Put '[JSON] (ApiResponse ())
  , _setOrderDimensions
      :: route
      :- "warehouse"
      :> "orders"
      :> "dimensions"
      :> RateLimitPerIP (Second 1)
      :> Capture "order_id" Text
      :> ReqBody '[JSON] SetOrderDimensionsRequest   
      :> Put '[JSON] (ApiResponse ())
   , _getSdekDeliveryPointUUID
       :: route
       :- "sdek"
       :> "delivery-point"
       :> RateLimitPerIP (Minute 10)
       :> QueryParam "address" Text
       :> Get '[JSON] (ApiResponse Text)
  } deriving (Generic)


tkaniApiProxy :: Proxy (ToServantApi Routes)
tkaniApiProxy = genericApi (Proxy :: Proxy Routes)