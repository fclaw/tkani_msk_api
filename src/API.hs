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


import API.Types
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
       :> ReqBody '[JSON] RawIngestRequest
       :> Put '[JSON] (ApiResponse NewFabric)
  , _patchFabric
       :: route
       :- "warehouse"
       :> "fabric"
       :> Capture "fabric_id" Int64
       :> ReqBody '[JSON] RawIngestRequest
       :> Patch '[JSON] (ApiResponse NewFabric)
  , _deleteFabric
       :: route
       :- "warehouse"
       :> "fabric"
       :> Capture "fabric_id" Int64
       :> Capture "fabric_type" FabricType
       :> Delete '[JSON] (ApiResponse ())  
  , _getFabricPreview 
       :: route 
       :- "fabric"
       :> "preview" 
       :> QueryParam "fabric_id" Int64
       :> QueryParam "fabric_type" FabricType
       :> Get '[JSON] (ApiResponse FabricPreview)
  , _getDeliveryPoints
       :: route 
       :- "providers"
       :> Capture "provider" Providers 
       :> "delivery-points" 
       :> QueryParam "city" Text 
       :> Get '[JSON] (ApiResponse [WithField "dpMetros" [Text] DeliveryPoint])
  , _getProviders
       :: route
       :- "providers"
       :> Get '[JSON] (ApiResponse [ProviderInfo])
  , _placeNewOrder
       :: route
       :- "order"
       :> "create"
       :> ReqBody '[JSON] OrderRequest
       :> Post '[JSON] (ApiResponse OrderConfirmationDetails)
  , _setTelegramMessage
       :: route
       :- "order"
       :> "set_telegram_message"
       :> ReqBody '[JSON] SetTelegramMessageRequest
       :> Post '[JSON] (ApiResponse ())
  , _trackOrder
       :: route
       :- "order"
       :> "track"
       :> QueryParam "query" Text
       :> Get '[JSON] (ApiResponse (Maybe TrackOrder))
  , _getCatalogSummary
       :: route
       :- "catalog"
       :> "by-date"
       :> QueryParam "date" CatalogDate
       :> Get '[JSON] (ApiResponse CatalogSummary)
   , _searchFabrics 
       :: route
       :- "search"
       :> QueryParam "query" Text
       :> QueryParam "page" Int
       :> QueryParam "limit" Int
       :> Get '[JSON] (ApiResponse (PaginatedResults SearchTeaser))
   , _searchFabricCard 
       :: route
       :- "search"
       :> Capture "type" FabricType
       :> Capture "id" Int64
       :> Get '[JSON] (ApiResponse (Maybe CatalogSummaryItem))
   , _putDailyDigest
       :: route
       :- "warehouse"
       :> "daily-digest"
       :> Post '[JSON] ()
  , _draftDailyDigestDraft
       :: route
       :- "warehouse"
       :> "daily-digest"
       :> "draft"
       :> ReqBody '[JSON] DailyDigestDraft
       :> Post '[JSON] (ApiResponse ())
  , _publishDailyDigest
       :: route
       :- "warehouse"
       :> "daily-digest"
       :> "publish"
       :> ReqBody '[JSON] DailyDigest
       :> Post '[JSON] (ApiResponse ())
  , _cancelOrder
       :: route
       :- "order"
       :> "cancel"
       :> ReqBody '[JSON] CancelOrder
       :> Post '[JSON] (ApiResponse ())
  , _checkCartItem
       :: route
       :- "cart"
       :> "check-item"
       :> QueryParam "user_id" Int64
       :> QueryParam "fabric_id" Int64
       :> QueryParam "fabric_type" FabricType
       :> Get '[JSON] (ApiResponse CheckItemInCart)
  , _addToCart 
       :: route
       :- "cart"
       :> "add"
       :> ReqBody '[JSON] CartNewFabric
       :> Post '[JSON] (ApiResponse CartCheckStatus)
  , _clearCart
       :: route
       :- "cart"
       :> "clear"
       :> QueryParam "user_id" Int64
       :> Get '[JSON] (ApiResponse ())
  , _viewCart
       :: route
       :- "cart"
       :> QueryParam "user_id" Int64
       :> Get '[JSON] (ApiResponse ViewCart)
  , _measureOrder
       :: route
       :- "warehouse"
       :> "orders"
       :> "measure"
       :> ReqBody '[JSON] MeasureRequest
       :> Post '[JSON] (ApiResponse MeasureResponse)
  , _placeYamlOrder
       :: route
       :- "warehouse"
       :> "orders"
       :> "create-from-yaml"
       :> ReqBody '[JSON] YamlOrderRequest
       :> Put '[JSON] (ApiResponse YamlOrderResponse)
  , _reportDailySales
      :: route
      :- "warehouse"
      :> "reports"
      :> "daily-sales"
      :> Post '[JSON] (ApiResponse ())
  , _reportMonthlySales
      :: route
      :- "warehouse"
      :> "reports"
      :> "monthly-sales"
      :> Post '[JSON] (ApiResponse ())
  , _tallyUpExpenses
      :: route
      :- "warehouse"
      :> "expenses"
      :> "tally-up"
      :> ReqBody '[JSON] Expenses
      :> Put '[JSON] (ApiResponse ())
  } deriving (Generic)


tkaniApiProxy :: Proxy (ToServantApi Routes)
tkaniApiProxy = genericApi (Proxy :: Proxy Routes)