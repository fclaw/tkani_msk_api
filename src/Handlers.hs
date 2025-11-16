{-# LANGUAGE OverloadedStrings #-}

module Handlers where

import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Servant.API.Generic (ToServant)
import Servant.Server.Generic (AsServerT)
import Katip (logTM, Severity(..))


import App (AppM) -- Your custom monad
import API (Routes (..)) -- Import our new ApiWithDocs
import qualified Handlers.GetFabricInfo as GetFabricInfo
import qualified Handlers.PutNewFabric as PutNewFabric
import qualified Handlers.GetDeliveryPoints as GetDeliveryPoints
import qualified Handlers.GetProviders as GetProviders
import qualified Handlers.PlaceNewOrder as PlaceNewOrder

-- This is the implementation of our server.
-- It's a record of handlers that matches the 'Routes' data type.
apiHandlers :: Routes (AsServerT AppM)
apiHandlers = Routes
  {  _getFabricInfo = GetFabricInfo.handler -- Assign the handler function to the field
  , _putNewFabric = PutNewFabric.handler
  , _getDeliveryPoints = GetDeliveryPoints.handler
  , _getProviders = GetProviders.handler
  , _placeNewOrder = PlaceNewOrder.handler
  }