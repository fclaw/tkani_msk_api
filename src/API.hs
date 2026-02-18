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
  , _uploadMediaForFabric
       :: route
       :- "warehouse"
       :> "fabric"
       :> "media"
       :> RateLimitPerUser (Second 1) 'Nothing
       :> ReqBody '[JSON] FabricMediaRequest
       :> Post '[JSON] (ApiResponse ())
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
   , _getCatalogSummary
       :: route
       :- "catalog"
       :> RateLimitPerUser (Second 1) 'Nothing
       :> QueryParam "life-cycle" FabricLifecycle
       :> Get '[JSON] (ApiResponse CatalogSummary)
     -- shelf API
   , _initShelf 
       :: route
       :- "shelf"
       :> RateLimitPerUser (Second 1) 'Nothing
       :> ReqBody '[JSON] (WithField "user_id" Int64 ShelfRequest)
       :> Put '[JSON] (ApiResponse ShelfIdResponse)
   , _fetchShelfItems
       :: route
       :- "shelf"
       :> RateLimitPerUser (Second 1) 'Nothing
       :> Capture "user_id" Int64
       :> Get '[JSON] (ApiResponse ShelfItemsResponse)
    , _putOnShelf
       :: route
       :- "shelf"
       :> "checkout"
       :> RateLimitPerUser (Second 1) 'Nothing
       :> Capture "user_id" Int64
       :> Post '[JSON] (ApiResponse PutOnShelfPaymentOptions)
    , _initiateShipment
       :: route
       :- "shelf"
       :> "ship"
       :> RateLimitPerUser (Second 1) 'Nothing
       :> Capture "user_id" Int64
       :> ReqBody '[JSON] InitiateShelfShipment
       :> Post '[JSON] (ApiResponse ShelfShipmentDetails)
    , _requestShelfStatus
       :: route
       :- "shelf"
       :> "status"
       :> RateLimitPerUser (Second 1) 'Nothing
       :> Capture "user_id" Int64
       :> Get '[JSON] (ApiResponse ShelfStatusResponse)
    , _saveShelfSubmissionInfo
       :: route
       :- "shelf"
       :> "submission"
       :> RateLimitPerUser (Second 1) 'Nothing
       :> ReqBody '[JSON] ShelfSubmissionChatDetails
       :> Post '[JSON] (ApiResponse ())
    , _getShelfPersonalInfo
       :: route
       :- "shelf"
       :> "personal-info"
       :> Capture "user_id" Int64
       :> RateLimitPerUser (Second 1) 'Nothing
       :> Get '[JSON] (ApiResponse ShelfPersonalInfo)
    , _editShelfPersonalInfo
       :: route
       :- "shelf"
       :> "personal-info"
       :> Capture "user_id" Int64
       :> RateLimitPerUser (Second 1) 'Nothing
       :> ReqBody '[JSON] ShelfPersonalInfo
       :> Patch '[JSON] (ApiResponse ())

    , _getSdekPointFullAddress
       :: route
       :- "sdek"
       :> "point"
       :> "full-address"
       :> QueryParam "code" Text
       :> RateLimitPerUser (Second 1) 'Nothing
       :> Get '[JSON] (ApiResponse (Maybe Text))

     , _getPreferredSdekPointWithAddress
       :: route
       :- "sdek"
       :> "point"
       :> "preferred"
       :> RateLimitPerUser (Second 1) 'Nothing
       :> Capture "user_id" Int64
       :> Get '[JSON] (ApiResponse (Maybe PreferredSdekPointWithAddress))

  } deriving (Generic)


tkaniApiProxy :: Proxy (ToServantApi Routes)
tkaniApiProxy = genericApi (Proxy :: Proxy Routes)