{-# LANGUAGE OverloadedStrings #-}

module Handlers where

import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Servant.API.Generic (ToServant)
import Servant.Server.Generic (AsServerT)
import Katip (logTM, Severity(..))


import App (AppM) -- Your custom monad
import API (Routes (..)) -- Import our new ApiWithDocs
import qualified API.Handlers.GetFabricPreview as GetFabricPreview
import qualified API.Handlers.PutNewFabric as PutNewFabric
import qualified API.Handlers.GetDeliveryPoints as GetDeliveryPoints
import qualified API.Handlers.GetProviders as GetProviders
import qualified API.Handlers.PlaceNewOrder as PlaceNewOrder
import qualified API.Handlers.SetTelegramMessage as SetTelegramMessage
import qualified API.Handlers.TrackOrder as TrackOrder
import qualified API.Handlers.GetCatalogSummary as GetCatalogSummary
import qualified API.Handlers.SearchFabrics as SearchFabrics
import qualified API.Handlers.SearchFabricCard as SearchFabricCard
import qualified API.Handlers.DailyDigest.New as DailyDigest.New
import qualified API.Handlers.DailyDigest.Draft as DailyDigest.Draft
import qualified API.Handlers.DailyDigest.Publish as DailyDigest.Publish
import qualified API.Handlers.CancelOrder as CancelOrder
import qualified API.Handlers.CheckCartItem as CheckCartItem
import qualified API.Handlers.AddToCart as AddToCart
import qualified API.Handlers.ClearCart as ClearCart
import qualified API.Handlers.ViewCart as ViewCart
import qualified API.Handlers.PatchFabric as PatchFabric
import qualified API.Handlers.DeleteFabric as DeleteFabric
import qualified API.Handlers.MeasureOrder as MeasureOrder
import qualified API.Handlers.PlaceYamlOrder as PlaceYamlOrder
import qualified API.Handlers.ReportDailySales as ReportDailySales
import qualified API.Handlers.ReportMonthlySales as ReportMonthlySales
import qualified API.Handlers.TallyUpExpenses as TallyUpExpenses
import qualified API.Handlers.SetOrderDimensions as SetOrderDimensions
import qualified API.Handlers.SdekDeliveryPointUUID as SdekDeliveryPointUUID



-- This is the implementation of our server.
-- It's a record of handlers that matches the 'Routes' data type.
apiHandlers :: Routes (AsServerT AppM)
apiHandlers = Routes
  { _getFabricPreview         = GetFabricPreview.handler -- Assign the handler function to the field
  , _putNewFabric             = PutNewFabric.handler
  , _getDeliveryPoints        = GetDeliveryPoints.handler
  , _getProviders             = GetProviders.handler
  , _placeNewOrder            = PlaceNewOrder.handler
  , _setTelegramMessage       = SetTelegramMessage.handler
  , _trackOrder               = TrackOrder.handler
  , _getCatalogSummary        = GetCatalogSummary.handler
  , _searchFabrics            = SearchFabrics.handler
  , _searchFabricCard         = SearchFabricCard.handler
  , _putDailyDigest           = DailyDigest.New.handler
  , _draftDailyDigestDraft    = DailyDigest.Draft.handler
  , _publishDailyDigest       = DailyDigest.Publish.handler
  , _cancelOrder              = CancelOrder.handler
  , _checkCartItem            = CheckCartItem.handler
  , _addToCart                = AddToCart.handler
  , _clearCart                = ClearCart.handler
  , _viewCart                 = ViewCart.handler
  , _patchFabric              = PatchFabric.handler
  , _deleteFabric             = DeleteFabric.handler
  , _measureOrder             = MeasureOrder.handler
  , _placeYamlOrder           = PlaceYamlOrder.handler
  , _reportDailySales         = ReportDailySales.handler
  , _reportMonthlySales       = ReportMonthlySales.handler
  , _tallyUpExpenses          = TallyUpExpenses.handler
  , _setOrderDimensions       = SetOrderDimensions.handler
  , _getSdekDeliveryPointUUID = SdekDeliveryPointUUID.handler
  }