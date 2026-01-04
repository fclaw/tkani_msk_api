{-# LANGUAGE OverloadedStrings #-}

module Handlers where

import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Servant.API.Generic (ToServant)
import Servant.Server.Generic (AsServerT)
import Katip (logTM, Severity(..))


import App (AppM) -- Your custom monad
import API (Routes (..)) -- Import our new ApiWithDocs
import qualified Handlers.GetFabricPreview as GetFabricPreview
import qualified Handlers.PutNewFabric as PutNewFabric
import qualified Handlers.GetDeliveryPoints as GetDeliveryPoints
import qualified Handlers.GetProviders as GetProviders
import qualified Handlers.PlaceNewOrder as PlaceNewOrder
import qualified Handlers.SetTelegramMessage as SetTelegramMessage
import qualified Handlers.TrackOrder as TrackOrder
import qualified Handlers.GetCatalogSummary as GetCatalogSummary
import qualified Handlers.SearchFabrics as SearchFabrics
import qualified Handlers.SearchFabricCard as SearchFabricCard
import qualified Handlers.DailyDigest.New as DailyDigest.New
import qualified Handlers.DailyDigest.Draft as DailyDigest.Draft
import qualified Handlers.DailyDigest.Publish as DailyDigest.Publish
import qualified Handlers.CancelOrder as CancelOrder
import qualified Handlers.CheckCartItem as CheckCartItem
import qualified Handlers.AddToCart as AddToCart
import qualified Handlers.ClearCart as ClearCart
import qualified Handlers.ViewCart as ViewCart
import qualified Handlers.PatchFabric as PatchFabric
import qualified Handlers.DeleteFabric as DeleteFabric
import qualified Handlers.MeasureOrder as MeasureOrder
import qualified Handlers.PlaceYamlOrder as PlaceYamlOrder
import qualified Handlers.ReportDailySales as ReportDailySales

-- This is the implementation of our server.
-- It's a record of handlers that matches the 'Routes' data type.
apiHandlers :: Routes (AsServerT AppM)
apiHandlers = Routes
  { _getFabricPreview = GetFabricPreview.handler -- Assign the handler function to the field
  , _putNewFabric = PutNewFabric.handler
  , _getDeliveryPoints = GetDeliveryPoints.handler
  , _getProviders = GetProviders.handler
  , _placeNewOrder = PlaceNewOrder.handler
  , _setTelegramMessage = SetTelegramMessage.handler
  , _trackOrder = TrackOrder.handler
  , _getCatalogSummary = GetCatalogSummary.handler
  , _searchFabrics = SearchFabrics.handler
  , _searchFabricCard = SearchFabricCard.handler
  , _putDailyDigest = DailyDigest.New.handler
  , _draftDailyDigestDraft = DailyDigest.Draft.handler
  , _publishDailyDigest = DailyDigest.Publish.handler
  , _cancelOrder = CancelOrder.handler
  , _checkCartItem = CheckCartItem.handler
  , _addToCart = AddToCart.handler
  , _clearCart = ClearCart.handler
  , _viewCart = ViewCart.handler
  , _patchFabric = PatchFabric.handler
  , _deleteFabric = DeleteFabric.handler
  , _measureOrder = MeasureOrder.handler
  , _placeYamlOrder = PlaceYamlOrder.handler
  , _reportDailySales = ReportDailySales.handler
  }